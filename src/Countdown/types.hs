module Countdown.Types where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Maybe (fromJust)
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
  show t@(Term op left right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ " = " ++ show (fromJust $ evaluate t) ++ ")"
  show (Single i) = show i

instance Ord Term where
  compare term term' = compare (op term) (op term') <> foldr (<>) EQ (zipWith compare (digits term) (digits term'))
    where
      op (Single _) = Plus
      op (Term o _ _) = o
      digits (Single i) = [i]
      digits (Term _ left right) = digits left ++ digits right

data Result = Result Term Natural Natural deriving (Eq)

instance Show Result where
  show (Result term result weight) = show result ++ " (" ++ show weight ++ ")" ++ " = " ++ show term

instance Ord Result where
  compare (Result term result weight) (Result term' result' weight') = compare result result' <> compare weight weight' <> compare term term'

evaluate' :: Term -> (Term, Maybe Natural, Natural)
evaluate' term = (term, evaluate term, size term)
  where
    size (Single _) = 1
    size (Term _ left right) = size left + size right

evaluate :: Term -> Maybe Natural
evaluate (Single i) = Just i
evaluate (Term Plus left right) = join $ liftA2 add (evaluate left) (evaluate right)
  where
    add x y | x >= y = Just $ x + y
    add _ _ = Nothing
evaluate (Term Minus left right) = join $ liftA2 minus (evaluate left) (evaluate right)
  where
    minus x y | x > y = Just $ x - y
    minus _ _ = Nothing
evaluate (Term Times left right) = join $ liftA2 times (evaluate left) (evaluate right)
  where
    times 1 _ = Nothing
    times _ 1 = Nothing
    times x y | x >= y = Just $ x * y
    times _ _ = Nothing
evaluate (Term Div left right) = join $ liftA2 div (evaluate left) (evaluate right)
  where
    div _ 0 = Nothing
    div _ 1 = Nothing
    div x y =
      let (d, m) = x `divMod` y
       in if m == 0 then Just d else Nothing