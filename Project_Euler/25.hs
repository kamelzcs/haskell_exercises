fibs = 0 : 1 : (zipWith (+) fibs $ tail fibs)

solve = length . takeWhile ((< 1000) . length . show)

main = print $ solve fibs


