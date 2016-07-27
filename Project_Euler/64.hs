import Data.List

sqrtFloor x = truncate (sqrt (fromIntegral x))

frac :: Integer -> [(Integer, (Integer, Integer))]
frac x = takeWhile ((/= 2 * (sqrtFloor x)) . fst) $ iterate f (a0, (0, 1))
  where a0 = sqrtFloor x
        f (a, (m, d)) = (a1, (m1, d1))
          where m1 = d * a - m
                d1 = (x - m1 * m1) `div` d
                a1 = (a0 + m1) `div` d1

main = print $ length $ filter (odd . length) $ map frac $ [2..10000] \\ map (^2) [2..100]
