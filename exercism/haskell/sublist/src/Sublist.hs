module Sublist (Sublist(..), sublist) where
import Data.List

-- The task is to create the data type `Sublist`, with `Eq` and
-- `Show` instances, and implement the function `sublist`.

data Sublist = Sublist | Superlist | Equal | Unequal deriving (Eq, Show)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist x y
  | x == y = Equal
  | x `isInfixOf` y = Sublist
  | y `isInfixOf` x = Superlist
  | otherwise = Unequal
