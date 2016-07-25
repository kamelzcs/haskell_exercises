square x = x * x

diag = 3: 5: 7: 9 : zipWith (+) diag [10, 12..]

toLength x = (x + 2) `div` 4 * 2 + 1

primeCount = scanl1 (+) $ map (\x -> if (isPrime x) then 1 else 0) diag

primes = 2 : filter isPrime [3, 5..]

isPrime x = null $ tail $ primeFactors x primes

primeFactors n (p : ps)
    | p * p > n = [n]
    | n `mod` p == 0 = p : primeFactors (n `div` p) (p : ps)
    | otherwise = primeFactors n ps


main = print $ toLength $ snd $ head $ dropWhile (\(p, n)-> 10 * p >= n) $ zip primeCount [2, 3..]
