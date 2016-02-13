import Data.List(tails)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y : ys | (y : xs') <- tails xs, ys <- combinations (pred n) xs']

main = print $ combinations 3 "abcdef"
