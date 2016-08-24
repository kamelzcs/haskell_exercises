module ETL (transform) where

import Data.Char
import Data.Map (Map, toList, fromList)

trans :: [(a, String)] -> [(Char, a)]
trans as = do
  (k, s) <- as
  do
    s' <- s
    return (toLower s', k)

transform :: Map a String -> Map Char a
transform = fromList . trans . toList
