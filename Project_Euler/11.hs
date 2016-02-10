import Control.Arrow
import Data.Array

dir = [(+1) *** id,(+1) *** (+1), id *** (+1), (+1) *** (\n -> n - 1)]

ar = listArray ((1, 1), (20, 20))

inArray a i = inRange (bounds a) i

solve :: Array (Int,Int) Int -> Int
solve a = maximum [product xs |
                  i <- range $ bounds a,
                  d <- dir,
                  let ps = take 4 $ iterate d i,
                  all (inArray a) ps,
                  let xs = map (a!) ps]

main = do
        str <- readFile "11.input"
        let ds = map read $ words str
        print $ solve $ ar ds
