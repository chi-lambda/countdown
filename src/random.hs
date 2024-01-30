module Random (next) where

import Data.Char (ord)
import Data.Functor ((<&>))
import System.IO (Handle, IOMode (ReadMode), hGetChar, hSetEncoding, latin1, openFile)

next :: Int -> Int -> IO Int
next from to = do
  (i1, i2) <- openFile "/dev/random" ReadMode |> (`hSetEncoding` latin1) >>= hGetChar2 <&> both ord
  let i = i1 * 256 + i2
  return $ i `mod` (to - from + 1) + from

hGetChar2 :: Handle -> IO (Char, Char)
hGetChar2 h = liftA2 (,) (hGetChar h) (hGetChar h)

both :: (t -> b) -> (t, t) -> (b, b)
both f (x, y) = (f x, f y)

(|>) :: IO b -> (b -> IO a) -> IO b
(|>) a f = a >>= f >> a
