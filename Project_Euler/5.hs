solve = foldr lcm 1 [1..20]

main = putStr $ show solve
