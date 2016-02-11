primes = 2 : filter isPrime [3, 5..]

isPrime x = null $ tail $ primeFactors x primes

primeFactors n (p : ps)
    | p * p > n = [n]
    | n `mod` p == 0 = p : primeFactors (n `div` p) (p : ps)
    | otherwise = primeFactors n ps

comOdds = filter (not . isPrime) [3, 5..]

validConject x = any isPrime $ takeWhile (>0) $ map (\d -> x - 2 * d * d) [1..]

conject = head $ filter (not . validConject) comOdds

main = print $ conject

