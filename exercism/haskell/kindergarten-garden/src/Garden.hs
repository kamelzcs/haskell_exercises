module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Prelude hiding(lookup)
import Data.List.Split (chunksOf)
import Data.List (transpose, sort)
import Data.Map (Map, lookup, fromList)
import Data.Maybe (fromJust)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

toPlant = fromJust . flip (lookup) (fromList [('C', Clover), ('G', Grass), ('R', Radishes), ('V', Violets)])

defaultGarden :: String -> Map String [Plant]
defaultGarden s = garden (take ((length s) `div` 2) dict) s
  where dict = ["Alice", "Bob", "Charlie", "David"
             ,"Eve", "Fred", "Ginny", "Harriet"
             ,"Ileana", "Joseph", "Kincaid", "Larry"]

garden :: [String] -> String -> Map String [Plant]
garden name plant = fromList $ zip (sort name) $ map (map toPlant) plants
  where plants = map concat $ transpose (map (chunksOf 2) (lines plant))

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants name m = fromJust $ lookup name m
