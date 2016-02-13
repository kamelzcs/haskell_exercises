
dropEvery xs n = Prelude.map snd $ filter ((>0) . (`mod` n) . fst) $ zip [1..] xs

main = print $ dropEvery "abcdefghik" 3
