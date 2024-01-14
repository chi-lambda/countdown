module Main (main) where

data Operation = Plus | Minus | Times | Div deriving (Eq)

instance Show Operation where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"

parse :: String -> [Int]
parse = map read . words

subdivide :: [Int] -> [([Int], [Int])]
subdivide numbers =
  let len = length numbers
      num = 2 ^ len :: Int
      bitsplit [] result _ = result
      bitsplit (x : xs) (r1, r2) n | even n = bitsplit xs (x : r1, r2) (n `div` 2)
      bitsplit (x : xs) (r1, r2) n = bitsplit xs (r1, x : r2) (n `div` 2)
      divisions = map (bitsplit numbers ([], [])) [1 .. num - 2]
   in divisions

main :: IO ()
main = interact $ show . subdivide . parse
