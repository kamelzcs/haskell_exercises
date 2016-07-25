square x = x * x

diag = 3: 5: 7: 9 : zipWith (+) diag [10, 12..]

toLength x = (x + 2) `div` 4 * 2 + 1

primeCount = scanl1 (+) $ map (\x -> if (isPrime x) then 1 else 0) diag
isPrime :: Integer -> Bool
isPrime x = go x 2
  where
  go x y
    | square y > x = True
    | ((== 0) (x `mod` y)) = False
    | otherwise = go x (1 + y)

main = print $ toLength $ snd $ head $ dropWhile (\(p, n)-> 10 * p >= n) $ zip primeCount [2, 3..]
