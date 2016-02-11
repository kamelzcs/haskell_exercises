import Data.Array
import qualified Data.Set as Set

primes = 2 : filter isPrime [3, 5..]

isPrime x = null $ tail $ primeFactors x primes

primeFactors n (p : ps)
    | p * p > n = [n]
    | n `mod` p == 0 = p : primeFactors (n `div` p) (p : ps)
    | otherwise = primeFactors n ps

primeFactorArray = listArray (2, 200000) [Set.fromList(primeFactors x primes) | x <- [2..200000]]

dpf x = all ((== 4) . Set.size) $ map (primeFactorArray !)[x, x + 1, x + 2, x + 3]


solve = head $ filter dpf [2..]

main = print $ solve
