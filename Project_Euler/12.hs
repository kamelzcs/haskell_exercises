import Data.List

primes = 2 : primes'
primes' = 3 : filter (isPrime primes') [5, 7 ..]
    where isPrime (p : ps) n = p * p > n || n `rem` p /= 0 && isPrime ps n

primeFactors n = primeFactors' primes n

primeFactors' a@(p : ps) n
    | p * p > n = [n]
    | n `rem` p == 0 = p : primeFactors' a (n `div` p)
    | otherwise = primeFactors' ps n

countPrimes n = product $ map ((+1) . length) (group $ primeFactors n)

solve = head $ filter ((>500) . countPrimes) $ scanl1 (+) [1..]

main = print solve
