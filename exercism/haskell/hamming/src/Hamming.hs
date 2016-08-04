module Hamming (distance) where

distance :: [Char] -> [Char] -> Maybe Int
distance x y
  | length x /= length y = Nothing
  | otherwise = Just $ sum $ zipWith (\x y -> if x == y then 0 else 1) x y
