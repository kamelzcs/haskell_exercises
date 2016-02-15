import Data.List

primes = 2 : filter isPrime [3, 5..]

isPrime x = null $ tail $ primeFactors x primes

primeFactors n (p : ps)
    | p * p > n = [n]
    | n `mod` p == 0 = p : primeFactors (n `div` p) (p : ps)
    | otherwise = primeFactors n ps

primeCandidates = takeWhile(<10000) $ dropWhile (<1000) primes

solve = [(a, b, c) | a <- primeCandidates,
        a /= 1487,
        b <- dropWhile (<= a) primeCandidates,
        sort (show a) == sort (show b),
        let c = 2 * b - a,
        c `elem` primeCandidates,
        sort (show a) == sort (show c)
        ]

main = print $ solve
