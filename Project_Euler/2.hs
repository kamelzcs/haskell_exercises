fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

solve = sum [x | x <- takeWhile (<= 4000000) fibs, even x]

main :: IO ()
main = do
        print solve
