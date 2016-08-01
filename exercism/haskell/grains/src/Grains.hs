module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
 | n > 64 || n <= 0 = Nothing
 | otherwise = Just $ iterate (*2) 1 !! (fromIntegral n - 1)

total = sum $ (map ((maybe 0 id) . square) [1..64])
