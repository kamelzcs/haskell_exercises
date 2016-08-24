module PrimeFactors (primeFactors) where

primes = 2 : filter isPrime [3, 5..]
  where isPrime = null . tail . primeFactors

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors x = go primes x
  where go d@(a : as) n | a * a > n = [n]
                        | n `mod` a == 0 = a : go d (n `div` a)
                        | otherwise = go as n
