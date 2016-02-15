import Data.List

facs = scanl (*) 1 [1..100]

combination (n, r) = facs !! n `div` ((facs !! r) * (facs !! (n - r)))

candidates = [(n, r) | n <- [1..100], r <- [1..(n - 1)]]

valid = (>1000000) . combination

solve = length $ filter valid candidates

main = print $ solve
