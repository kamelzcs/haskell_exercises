module Minesweeper (annotate) where

import Data.Ix
import Data.Array
import Control.Arrow
import Data.List
import Data.List.Split
import Data.Char

annotate :: [String] -> [String]
annotate [] = []
annotate ms = toList (matrix // [((x, y), intToDigit e) | ((x, y), c) <- assocs matrix, (matrix ! (x, y)) /= '*', let e = countNear matrix (x,y), e /=0 ])
  where matrix = buildArray ms


dir =[(subtract 1 *** id), subtract 1 *** (+1), id *** (+1), ((+1) *** (+1)),
      ((+1) *** id), (+1) *** subtract 1, id *** subtract 1, subtract 1 *** subtract 1]

buildArray :: [String] -> Array (Int, Int) Char
buildArray matrix = array ((0,0), (height - 1, width - 1)) $ zipWith (,) ranges chars
  where width = length $ head matrix
        height = length matrix
        ranges = range ((0,0), (height - 1, width - 1))
        chars = concat matrix

countNear :: Array (Int, Int) Char -> (Int, Int) -> Int
countNear matrix pos = length $ filter valid dir
  where valid delta = let npos = (delta pos) in inRange (bounds matrix) npos && ((matrix ! npos ) == '*')

toList :: Array (Int, Int) Char -> [String]
toList a = chunksOf width $ elems a
  where width = (+ 1) $ snd $ snd $ bounds a
