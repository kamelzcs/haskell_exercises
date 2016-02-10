parse :: String -> [[Int]]
parse s  = map (map read) (map words $ lines s)

g x y = zipWith (+) x $ zipWith max y $ tail y

solve ints = foldr1 g ints

main = do
        strs <- readFile "18.input"
        print $ solve $ parse strs
