main = print $ length $ [y | x <- [1..9], y <- [1..100], y == (length $ show $ x^y)]
