import Data.List
import Data.Char


transform = map (fromIntegral . digitToInt) . concat . lines

maxProd :: (Num a, Ord a) => Int -> [a] -> a
maxProd n = maximum
          . map product 
          . takeWhile ((== n) . length)
          . map (take n)
          . tails

main = do
        xs <- readFile "number.txt"
        print $ maxProd 13 $ transform xs

