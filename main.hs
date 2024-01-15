module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Array.IArray (Array, (!))
import Data.Array.IArray qualified as A
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

data Term = Term Operation Term Term | Single Natural deriving (Eq)

instance Show Term where
  show (Term op left right) = "(" ++ show left ++ show op ++ show right ++ ")"
  show (Single i) = show i

instance Ord Term where
  compare term term' = compare (op term) (op term') <> foldr (<>) EQ (zipWith compare (digits term) (digits term'))
    where
      op (Single _)= Plus
      op (Term o _ _) = o
      digits (Single i) = [i]
      digits (Term _ left right) = digits left ++ digits right

data Result = Result Term Natural Natural deriving (Eq)

instance Show Result where
  show (Result term result _) = show result ++ " = " ++ show term

instance Ord Result where
  compare (Result term result weight) (Result term' result' weight') = compare result result' <> compare weight weight' <> compare term term'

evaluate' :: Term -> (Term, Maybe Natural, Natural)
evaluate' term = (term, evaluate term, size term)
  where
    evaluate (Single i) = Just i
    evaluate (Term Plus left right) = join $ liftA2 add (evaluate left) (evaluate right)
      where
        add x y | x > y = Just $ x + y
        add _ _ = Nothing
    evaluate (Term Minus left right) = join $ liftA2 minus (evaluate left) (evaluate right)
      where
        minus x y | x > y = Just $ x - y
        minus _ _ = Nothing
    evaluate (Term Times left right) = join $ liftA2 times (evaluate left) (evaluate right)
      where
        times 1 _ = Nothing
        times _ 1 = Nothing
        times x y | x > y = Nothing
        times x y = Just $ x * y
    evaluate (Term Div left right) = join $ liftA2 div (evaluate left) (evaluate right)
      where
        div _ 0 = Nothing
        div _ 1 = Nothing
        div x y =
          let (d, m) = x `divMod` y
           in if m == 0 then Just d else Nothing

parse :: String -> (Natural, [Natural])
parse = headTail . map read . words
  where
    headTail xs = (head xs, tail xs)

subdivide :: [Natural] -> [([Natural], [Natural])]
subdivide numbers =
  let len = length numbers
      num = 2 ^ len :: Int
      bitsplit [] result _ = result
      bitsplit (x : xs) (r1, r2) n = let (d, m) = n `divMod` 2 in if even m then bitsplit xs (x : r1, r2) d else bitsplit xs (r1, x : r2) d
      divisions = map (bitsplit numbers ([], [])) [1 .. num - 2]
   in divisions

terms :: [Natural] -> [Term]
terms [x] = [Single x]
terms xs = [Single i | i <- xs] ++ [Term op leftTerm rightTerm | op <- [Plus .. Div], (left, right) <- subdivide xs, leftTerm <- terms left, rightTerm <- terms right]

size :: Term -> Natural
size (Single _) = 1
size (Term _ left right) = size left + size right

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (x, y, z) = f x y z

catSndMaybes :: [(a, Maybe b, c)] -> [(a, b, c)]
catSndMaybes [] = []
catSndMaybes ((_, Nothing, _) : xs) = catSndMaybes xs
catSndMaybes ((a, Just fa, s) : xs) = (a, fa, s) : catSndMaybes xs

firstNonEmpty :: Map (Natural, Natural) (Set Result) -> Maybe (Set Result)
firstNonEmpty s =
  case M.minView s of
    Just (m, rest) -> if S.null m then firstNonEmpty rest else Just m
    Nothing -> Nothing

-- solve :: Natural -> [Natural] -> Maybe Result
solve :: Natural -> [Natural] -> Maybe [Result]
solve target numbers =
  let ts = terms numbers
      absDiff x y | x > y = x - y
      absDiff x y = y - x
      results = map (uncurry3 Result) $ filter ((<= 10) . absDiff target . snd3) $ catSndMaybes $ map evaluate' ts
      byScore = [((absDiff target result, weight), r) | r@(Result _ result weight) <- results]
      arr = A.accumArray (flip S.insert) S.empty ((0, 1), (10, 6)) byScore :: Array (Natural, Natural) (Set Result)
      mapped = M.fromList [((score, weight), arr ! (score, weight)) | score <- [0 .. 10], weight <- [1 .. 6]]
   in S.toList <$> firstNonEmpty mapped

showMaybeList :: Maybe [Result] -> String
showMaybeList Nothing = "No solution"
showMaybeList (Just xs) = unlines $ map show xs

main :: IO ()
main = interact $ showMaybeList . uncurry solve . parse