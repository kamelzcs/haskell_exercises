module OCR (convert) where

import Prelude hiding (lookup)
import Data.Map (fromList, lookup, findWithDefault)
import Data.Maybe (fromJust)
import Data.List (splitAt, intersperse)
import Data.List.Split

dict = fromList [(
                     [ " _ "
                     , "| |"
                     , "|_|"
                     , "   " ], "0"),
                  (
                     [ "   "
                     , "  |"
                     , "  |"
                     , "   " ], "1"),
                  (
                     [ " _ "
                     , " _|"
                     , "|_ "
                     , "   "], "2"),
                  (
                     [ " _ "
                     , " _|"
                     , " _|"
                     , "   " ], "3"),
                  (
                     [ "   "
                     , "|_|"
                     , "  |"
                     , "   " ], "4"),
                  (
                     [ " _ "
                     , "|_ "
                     , " _|"
                     , "   " ], "5"),
                  (
                     [ " _ "
                     , "|_ "
                     , "|_|"
                     , "   "], "6"),
                  (
                     [ " _ "
                     , "  |"
                     , "  |"
                     , "   " ], "7"),
                  (
                     [ " _ "
                     , "|_|"
                     , "|_|"
                     , "   " ], "8"),
                  (
                     [ " _ "
                     , "|_|"
                     , " _|"
                     , "   "], "9")]

convert' :: [String] -> String
convert' s = concat . map (flip (findWithDefault "?") dict)  $ splitCharacter' s

splitCharacter' :: [String] -> [[String]]
splitCharacter' xs | any (== "") xs = []
                  | otherwise = h : splitCharacter' remain
  where h = [w | s <- xs, let w = fst $ splitAt 3 s]
        remain = [r | s <- xs, let r = snd $ splitAt 3 s]

convert xs = concat $ intersperse "," $ map convert' $ chunksOf 4 $ lines xs
