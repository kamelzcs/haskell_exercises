import Data.List
import Data.Function

main = print . (^3) . snd $head $ head $ dropWhile ((/=5) . length) $ groupBy (\(x1, y1) (x2, y2) -> x1 == x2) cubes
  where cubes = sortBy (compare `on` fst) $ zipWith (,) (map (sort . show . (^3)) [2, 3..10000]) [2, 3..]
