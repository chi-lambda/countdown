module Main (main) where

import Data.Char (ord)
import Data.Foldable (foldrM)
import System.IO (IOMode (ReadMode), hGetChar, hSetEncoding, latin1, openFile)

smallNumbers :: [Int]
smallNumbers = [1 .. 10] ++ [1 .. 10]

bigNumbers :: [Int]
bigNumbers = [25, 50, 75, 100]

data Countdown = Countdown Int [Int]

instance Show Countdown where
  show (Countdown target nums) = unwords (map show nums) ++ "\n" ++ show target

remove :: [Int] -> Int -> (Int, [Int])
remove l n =
  let (h, t : ts) = splitAt n l
   in (t, h ++ ts)

getNumbers :: IO Countdown
getNumbers = do
  let next from to = do
        h <- openFile "/dev/random" ReadMode
        hSetEncoding h latin1
        i1 <- ord <$> hGetChar h
        i2 <- ord <$> hGetChar h
        let i = i1 * 256 + i2
        return $ i `mod` (to - from + 1) + from
      foldF _ (r, rest) =
        next 0 (length rest - 1) >>= \i ->
          let (n, rest') = remove rest i
           in return (n : r, rest')
  bigCount <- next 0 4
  let smallCount = 6 - bigCount
  bigs <-
    if bigCount == 4
      then return bigNumbers
      else fst <$> foldrM foldF ([], bigNumbers) [1 .. bigCount]
  smalls <- fst <$> foldrM foldF ([], smallNumbers) [1 .. smallCount]
  target <- next 100 999
  return $ Countdown target (bigs ++ smalls)

main :: IO ()
main = getNumbers >>= print
