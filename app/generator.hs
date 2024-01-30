module Generator (main) where

import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Random (next)

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
  bigCount <- next 0 4
  let smallCount = 6 - bigCount
  bigs <-
    if bigCount == 4
      then return bigNumbers
      else fst <$> foldrM foldF ([], bigNumbers) [1 .. bigCount]
  smalls <- fst <$> foldrM foldF ([], smallNumbers) [1 .. smallCount]
  target <- next 100 999
  return $ Countdown target (bigs ++ smalls)
  where
    foldF _ (r, rest) =
      next 0 (length rest - 1) <&> \i ->
        let (n, rest') = remove rest i
         in (n : r, rest')

main :: IO ()
main = getNumbers >>= print
