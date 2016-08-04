module SumOfMultiples (nthSuperUglyNumber) where

generateMuls :: [Integer] -> [Integer]
generateMuls as = ds
  where
    ds = 1 : (merge $ map (\x -> map (*x) ds) as)

merge :: [[Integer]] -> [Integer]
merge ints = minV : merge left
  where minV = minimum $ map head ints
        left = do
            vs <- ints
            if (head vs == minV) then (return $ tail vs) else (return vs)

nthSuperUglyNumber ::  Int -> [Integer] -> Integer
nthSuperUglyNumber n as = (generateMuls as ) !! (n - 1)
