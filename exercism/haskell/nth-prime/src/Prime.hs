module Prime (nth) where

primes = 2: filter isPrime [3, 5..]
  where isPrime x = null $ tail $ primeFactor x primes

primeFactor v p@(x : xs) | x * x > v = [v]
                         | (v `mod` x) == 0 = x : primeFactor (v `div` x) p

                         | otherwise = primeFactor v xs

nth n = primes !! (pred n)
