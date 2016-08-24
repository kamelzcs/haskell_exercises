module Allergies (Allergen(..), allergies, isAllergicTo) where

-- The task is to create the data type `Allergen`, with `Eq`
-- and `Show` instances, and implement the functions below.

import Data.Bits
import Data.List

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes | Chocolate | Pollen | Cats deriving (Eq, Ord, Show)

dict = [(Eggs, 1), (Peanuts, 2), (Shellfish, 4), (Strawberries, 8), (Tomatoes, 16), (Chocolate, 32), (Pollen, 64), (Cats, 128)]

allergies :: Int -> [Allergen]
allergies n = map fst $ filter (\(v, d) -> (n .&. d) > 0) dict

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo v n = elem v $ allergies n
