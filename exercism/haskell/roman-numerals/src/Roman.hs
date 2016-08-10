module Roman (numerals) where

import Prelude hiding (lookup)
import Data.Map(fromList, lookup, keys)
import Data.Maybe
import Data.List(foldl')


dict = fromList [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]


numerals :: Int -> String
numerals n = fst $ foldl' (\(s, t) k -> (s ++ (concat $ replicate (t `div` k) $ fromJust $ lookup k dict), t - (t `div` k) * k)) ("", n) $ reverse (keys dict)
