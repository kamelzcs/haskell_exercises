slice xs l r = [x | (i, x) <- zip [1..] xs, i >= l, i <= r]

main = print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
