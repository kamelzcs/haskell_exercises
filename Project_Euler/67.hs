
go :: [[Integer]] -> [Integer]
go = foldr1 (\x y -> zipWith (+) x (zipWith max y (tail y)))

parse :: String -> [[Integer]]
parse = map (map read . words) . lines

main = readFile "triangle.txt" >>= print . head . go . parse
