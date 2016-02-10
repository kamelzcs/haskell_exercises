
main = do
    xs <- readFile "13.input"
    let ds = map read $ lines xs
    print $ take 10 $ show $ sum ds

