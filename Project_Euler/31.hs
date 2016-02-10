coins = [1,2,5,10,20,50,100,200]

ways :: Int -> Int -> Int
ways 1 _ = 1
ways n x = sum $ map use [0 .. x `div` coins !! (n - 1)]
    where use k = ways (n - 1) (x - k * coins !! (n - 1))

main = print $ ways (length coins) 200
