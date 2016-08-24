module CryptoSquare
    ( ciphertext
    , normalizeCiphertext
    , normalizePlaintext
    , plaintextSegments
    , squareSize
    ) where

import Data.Char(isAlphaNum, toLower)
import Data.List.Split
import Data.List


ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

splitLength r l = replicate (total - remain) r ++ replicate remain (r - 1)
  where total = (l + r - 1) `div` r
        remain = r * total - l

normalizeCiphertext :: String -> String
normalizeCiphertext s = normalizeCiphertext' $ ciphertext s
normalizeCiphertext' s = unwords $ splitPlaces (splitLength r l) s
  where r = if (sz ^ 2 == l) then sz else sz - 1
        sz = squareSize s
        l = length s

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isAlphaNum

squareSize :: String -> Int
squareSize s = head $ dropWhile (\x -> (x ^ 2) < ls) [1,2..]
  where ls = length s

plaintextSegments :: String -> [String]
plaintextSegments s = plaintextSegments' $ normalizePlaintext s

plaintextSegments' s = splitPlaces (unfoldr (\l -> if (l <= 0) then Nothing else let v = minimum [l, sz] in Just (v, l - v)) ls) s
  where ls = length s
        sz = squareSize s
