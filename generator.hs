module Main (main) where

import System.Random (RandomGen, initStdGen, uniformR)

smallNumbers :: [Int]
smallNumbers = [1 .. 10] ++ [1 .. 10]

bigNumbers :: [Int]
bigNumbers = [25, 50, 75, 100]

data Countdown = Countdown Int [Int]

instance Show Countdown where
  show (Countdown target nums) = unwords (map show nums) ++ " " ++ show target

remove :: [Int] -> Int -> (Int, [Int])
remove l n =
  let ~(h, t : ts) = splitAt n l
   in (t, h ++ ts)

getNumbers :: IO Countdown
getNumbers = do
  rnd <- initStdGen
  let (bigCount, rnd') = uniformR (1, 4) rnd
  let smallCount = 6 - bigCount
  let (rnd'', bigs, _) =
        if bigCount == 4
          then (rnd, bigNumbers, [])
          else foldr foldF (rnd', [], bigNumbers) [1 .. bigCount]
  let (rnd''', smalls, _) = foldr foldF (rnd'', [], smallNumbers) [1 .. smallCount]
  let (target, _) = uniformR (100, 999) rnd'''
  return $ Countdown target (bigs ++ smalls)
  where
    foldF :: (RandomGen g) => Int -> (g, [Int], [Int]) -> (g, [Int], [Int])
    foldF _ (rnd, r, rest) =
      let (i, rnd') = uniformR (0, length rest - 1) rnd
          (n, rest') = remove rest i
       in (rnd', n : r, rest')

main :: IO ()
main = getNumbers >>= print
