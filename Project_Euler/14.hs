import Data.Array
import Data.List
import Data.Ord (comparing)
import Control.Arrow

sq :: Integer -> Array Integer Integer
sq n = a
    where a = listArray (1, n) $ 0 : map seq' [2..n]
          seq' x = if y <= n then 1 + a ! y else 1 + seq' y
                            where y = if even x then x `div` 2 else 3 * x + 1

solve :: Integer -> (Integer, Integer)
solve = foldl1' (\ x y -> if snd x > snd y then x else y) . assocs .sq

main = print $ solve 1000000

{-http://stackoverflow.com/questions/32551255/questions-regarding-haskell-solution-of-project-euler-14-}
{-http://stackoverflow.com/questions/7140732/haskell-space-overflow-}

