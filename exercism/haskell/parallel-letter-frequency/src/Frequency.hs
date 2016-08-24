module Frequency (frequency) where

import Data.Map  (Map, unionWith, empty, insertWith)
import Data.Text (Text, chunksOf, foldl', filter, concat, length, toLower)
import Control.Parallel.Strategies
import qualified Data.List as List (foldl')
import Data.Char (isAlpha)
import Prelude hiding (foldl', filter, concat, length)

-- | Given a number workers to use in parallel and a list of
-- texts, returns the total frequency of each letter in the text.
frequency :: Int -> [Text] -> Map Char Int
frequency n text = let texts = splits n text
                       results = parMap rpar solve texts
                   in List.foldl' (unionWith (+)) empty results

splits :: Int -> [Text] -> [Text]
splits n ts = chunksOf l t
  where t = concat ts
        l = ((length t) + (n - 1)) `div` n

solve :: Text -> Map Char Int
solve t = foldl' (\m c -> insertWith (+) c 1 m) empty (toLower $ filter isAlpha t)
