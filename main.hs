module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.List (sortOn)
import GHC.Natural (minusNaturalMaybe)
import Numeric.Natural (Natural)

data Operation = Plus | Minus | Times | Div deriving (Eq, Enum)

instance Show Operation where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"

data Term = Term Operation Term Term | Single Natural deriving (Eq)

instance Show Term where
  show (Term op left right) = "(" ++ show left ++ show op ++ show right ++ ")"
  show (Single i) = show i

maybeDiv :: Natural -> Natural -> Maybe Natural
maybeDiv _ 0 = Nothing
maybeDiv x y =
  let (d, m) = x `divMod` y
   in if m == 0 then Just d else Nothing

evaluate :: Term -> Maybe Natural
evaluate (Single i) = Just i
evaluate (Term Plus left right) = liftA2 (+) (evaluate left) (evaluate right)
evaluate (Term Minus left right) = join $ liftA2 minusNaturalMaybe (evaluate left) (evaluate right)
evaluate (Term Times left right) = liftA2 (*) (evaluate left) (evaluate right)
evaluate (Term Div left right) = join $ liftA2 maybeDiv (evaluate left) (evaluate right)

parse :: String -> (Natural, [Natural])
parse = headTail . map read . words
  where
    headTail (x : xs) = (x, xs)

subdivide :: [Natural] -> [([Natural], [Natural])]
subdivide numbers =
  let len = length numbers
      num = 2 ^ len :: Int
      bitsplit [] result _ = result
      bitsplit (x : xs) (r1, r2) n | even n = bitsplit xs (x : r1, r2) (n `div` 2)
      bitsplit (x : xs) (r1, r2) n = bitsplit xs (r1, x : r2) (n `div` 2)
      divisions = map (bitsplit numbers ([], [])) [1 .. num - 2]
   in divisions

terms :: [Natural] -> [Term]
terms [x] = [Single x]
terms xs = [Term op leftTerm rightTerm | op <- [Plus .. Div], (left, right) <- subdivide xs, leftTerm <- terms left, rightTerm <- terms right]

toSnd :: (t -> b) -> t -> (t, b)
toSnd f a = (a, f a)

catSndMaybes :: [(a, Maybe b)] -> [(a, b)]
catSndMaybes [] = []
catSndMaybes ((_, Nothing) : xs) = catSndMaybes xs
catSndMaybes ((a, Just fa) : xs) = (a, fa) : catSndMaybes xs

solve :: Natural -> [Natural] -> [(Term, Natural)]
solve target numbers =
  let ts = terms numbers
      filterF x | x > target = x - target <= 10
      filterF x = target - x <= 10
      sortF x | x > target = x - target
      sortF x = target - x
      ps = sortOn (sortF . snd) $ filter (filterF . snd) $ catSndMaybes $ map (toSnd evaluate) ts
   in take 5 ps

main :: IO ()
main = interact $ show . uncurry solve . parse
