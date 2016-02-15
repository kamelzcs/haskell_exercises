import Data.Ord
import Data.List

primes = 2 : filter isPrime [3, 5..]

isPrime x = null $ tail $ primeFactors x primes

primeFactors n (p : ps)
    | p * p > n = [n]
    | n `mod` p == 0 = p : primeFactors (n `div` p) (p : ps)
    | otherwise = primeFactors n ps

primeCandidates = takeWhile (<1000000) primes

allCandidates :: [[(Integer, Integer)]]
allCandidates = Prelude.map (zip [1..]) $ tails primeCandidates

sequenceCandidates :: [[(Integer, Integer)]]
sequenceCandidates = Prelude.map  (takeWhile (\(_, s) -> s < 1000000) . scanl1 (\(_, s) (i, s2) -> (i, s + s2)))  allCandidates

validCandidates = filter isValid sequenceCandidates

isValid = not . null . filter (\(_, v) -> isPrime v)

solve = maximumBy (comparing fst) $ Prelude.map  (last . filter (\(_, v) -> isPrime v)) validCandidates

main = print $ solve
