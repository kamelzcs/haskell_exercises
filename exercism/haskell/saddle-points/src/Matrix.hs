module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))
import Data.Ix (range)

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = [(a, b) | a <- range(x0, x1), b <- range(y0, y1), (saddle a b)]
  where ((x0,y0), (x1, y1)) = bounds matrix
        saddle x y = all (\(a, b) -> (matrix!(a, b) <= matrix!(x, y))) [(x, y') | y' <- range(y0, y1), y' /= y] && all (\(a, b) -> (matrix!(a, b) >= matrix!(x, y))) [(x', y) | x' <- range(x0, x1), x' /= x]
