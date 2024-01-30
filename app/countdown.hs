{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Countdown (main) where

import Data.Array.IArray (Array, (!))
import Data.Array.IArray qualified as A
import Data.Bifunctor (bimap)
import Data.Ix (Ix)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Numeric.Natural (Natural)
import Prelude hiding (div)

data Operation = Plus | Minus | Times | Div deriving (Eq, Enum, Ord)

instance Show Operation where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"

data Term = Term Operation Term Term Natural | Single CDNum deriving (Eq)

instance Show Term where
  show (Term op left right v) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ " = " ++ show v ++ ")"
  show (Single i) = show i

instance Ord Term where
  compare (Single i) (Single j) = compare i j
  compare (Single _) (Term {}) = LT
  compare (Term {}) (Single _) = GT
  compare (Term op left right _) (Term op' left' right' _) = compare op op' <> compare left left' <> compare right right'

newtype CDNum = CDNum Natural deriving (Eq, Ord, Num)

instance Show CDNum where
  show (CDNum i) = show i

instance Enum CDNum where
  succ n | n <= CDNum 10 = n + CDNum 1
  succ 10 = 25
  succ 25 = 50
  succ 50 = 75
  succ 75 = 100
  succ i = error $ "invalid CountdownNumber " ++ show i ++ " + 1"
  pred 100 = 75
  pred 75 = 50
  pred 50 = 25
  pred 25 = 10
  pred n | n <= 10 = n - 1
  pred i = error $ "invalid CountdownNumber " ++ show i ++ " - 1"
  toEnum n | n <= 10 = fromIntegral n
  toEnum 11 = CDNum 25
  toEnum 12 = CDNum 50
  toEnum 13 = CDNum 75
  toEnum 14 = CDNum 100
  toEnum n = error $ "invalid index " ++ show n
  fromEnum (CDNum i) | i <= 10 = fromIntegral i
  fromEnum (CDNum 25) = 11
  fromEnum (CDNum 50) = 12
  fromEnum (CDNum 75) = 13
  fromEnum (CDNum 100) = 14
  fromEnum i = error $ "invalid CountdownNumber " ++ show i

instance Ix CDNum where
  range (l, u) = [l .. u]
  index (m, _n) i = fromEnum i - fromEnum m
  inRange (l, u) m = l <= m && m <= u

data Result = Result Term Natural Natural deriving (Eq)

instance Show Result where
  show (Result term result weight) = show result ++ " (" ++ show weight ++ ")" ++ " = " ++ show term

instance Ord Result where
  compare (Result term result weight) (Result term' result' weight') = compare result result' <> compare weight weight' <> compare term term'

value :: Term -> Natural
value (Single (CDNum i)) = i
value (Term _ _ _ v) = v

evaluate :: Operation -> Term -> Term -> Maybe Natural
evaluate Plus left right = add (value left) (value right)
  where
    add x y | x >= y = Just $ x + y
    add _ _ = Nothing
evaluate Minus left right = minus (value left) (value right)
  where
    minus x y | x > y && x - y /= y = Just $ x - y
    minus _ _ = Nothing
evaluate Times left right = times (value left) (value right)
  where
    times 1 _ = Nothing
    times _ 1 = Nothing
    times x y | x >= y = Just $ x * y
    times _ _ = Nothing
evaluate Div left right = div (value left) (value right)
  where
    div _ 0 = Nothing
    div _ 1 = Nothing
    div x y =
      let (d, m) = x `divMod` y
       in if m == 0 && d /= y then Just d else Nothing

toResult :: Term -> Result
toResult t@(Single (CDNum i)) = Result t i 1
toResult t@(Term _ _ _ v) = Result t v (size t)
  where
    size (Single _) = 1
    size (Term _ left right _) = size left + size right

parse :: String -> (Natural, [CDNum])
parse = initLast . map read . words
  where
    initLast xs = (last xs, map CDNum $ init xs)

firstNonEmpty :: Map (Natural, Natural) (Set Result) -> Maybe (Set Result)
firstNonEmpty s =
  case M.minView s of
    Just (m, rest) -> if S.null m then firstNonEmpty rest else Just m
    Nothing -> Nothing

{-# INLINE toTuple #-}
toTuple :: (Num a) => [a] -> (a, a, a, a, a)
toTuple [a] = (a, 0, 0, 0, 0)
toTuple [a, b] = (a, b, 0, 0, 0)
toTuple [a, b, c] = (a, b, c, 0, 0)
toTuple [a, b, c, d] = (a, b, c, d, 0)
toTuple [a, b, c, d, e] = (a, b, c, d, e)
toTuple xs = error $ "toTuple: list of length " ++ show (length xs)

terms :: [CDNum] -> [Term]
terms = terms'
  where
    bounds = ((CDNum 0, CDNum 0, CDNum 0, CDNum 0, CDNum 0), (CDNum 100, CDNum 100, CDNum 100, CDNum 100, CDNum 100))
    range = [CDNum 0 .. CDNum 100]
    memo :: Array (CDNum, CDNum, CDNum, CDNum, CDNum) [Term]
    memo = A.array bounds [((a, b, c, d, e), terms' (filter (/= CDNum 0) [a, b, c, d, e])) | a <- range, b <- range, c <- range, d <- range, e <- range]
    terms'' = (memo !) . toTuple
    terms' :: [CDNum] -> [Term]
    terms' [i] = [Single i]
    terms' xs =
      [Single i | i <- xs]
        ++ [ Term op leftTerm rightTerm v
             | op <- [Plus .. Div],
               (left, right) <- subdivide xs,
               leftTerm <- terms'' left,
               rightTerm <- terms'' right,
               Just v <- [evaluate op leftTerm rightTerm]
           ]

subdivide :: [CDNum] -> [([CDNum], [CDNum])]
subdivide numbers' =
  let len = length numbers'
      num = 2 ^ len :: Natural
      bitsplit [] result _ = bimap sort sort result
      bitsplit (x : xs) (r1, r2) n = let (q, m) = n `divMod` 2 in if even m then bitsplit xs (x : r1, r2) q else bitsplit xs (r1, x : r2) q
      divisions = map (bitsplit numbers' ([], [])) [1 .. num - 2]
   in divisions

solve :: Natural -> [CDNum] -> Maybe [Result]
solve target numbers =
  let ts = terms numbers
      d x y | x > y = x - y
      d x y = y - x
      result (Result _ r _) = r
      results = filter ((<= 10) . d target . result) $ map toResult ts
      byScore = [((d target val, weight), r) | r@(Result _ val weight) <- results]
      arr = A.accumArray (flip S.insert) S.empty ((0, 1), (10, 6)) byScore :: Array (Natural, Natural) (Set Result)
      mapped = M.fromList [((score, weight), arr ! (score, weight)) | score <- [0 .. 10], weight <- [1 .. 6]]
   in S.toList <$> firstNonEmpty mapped

showMaybeList :: Maybe [Result] -> String
showMaybeList Nothing = "No solution"
showMaybeList (Just xs) = unlines $ map show xs

main :: IO ()
main = interact $ unlines . map (showMaybeList . uncurry solve . parse) . lines
