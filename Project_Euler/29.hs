import Data.Set

solve a b = size $ fromList [x^y | x <- [2..a], y <- [2..b]]

main = print $ solve 100 100
