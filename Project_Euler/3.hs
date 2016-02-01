
primes :: [Int] -> [Int]
primes (x : xs) = x : primes (filter (\v -> v `mod` x /= 0) xs)

primeFactors :: Int -> [Int] -> [Int]
primeFactors n l@(p : ps)
    | p * p > n = [n]
    | n `mod` p == 0 = p : primeFactors (n `div` p) l
    | otherwise = primeFactors n ps

main :: IO()
main = putStr $ show $ last $ primeFactors 600851475143 $ primes [2..]


