frac :: Integer -> [(Integer, (Integer, Integer))]
frac x = takeWhile ((/=2) . fst) iterate f (a0, (0, 1))
  where a0 = truncate (sqrt (fromIntegral x))
        f (a, (m, d)) = (a1, (m1, d1))
          where m1 = d * a - m
                d1 = (x - m1 * m1) `div` d
                a1 = (a0 + m1) `div` d1

main = print $ length $ filter (even . length) $ map frac [2..10000]
