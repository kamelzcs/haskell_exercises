import Data.List
import Data.Ord

cy :: Int -> Int
cy d = cycle' d 10 []

cycle' :: Int -> Int -> [Int] -> Int
cycle' _ 0 _ = 0
cycle' d r rs = let r' = r `rem` d
                in case elemIndex r' rs of
                       Just i -> i + 1
                       Nothing -> cycle' d (10 * r') $ r' : rs

solve :: Int -> Int
solve n = fst $ maximumBy (comparing snd) [(x, cy x) | x <- [2..n]]

main = print $ solve 999
