combines :: Int -> [a] -> [([a], [a])]
combines 0 x = [([], x)]
combines _ [] = []
combines n (x : xs) = as ++ ds
  where as = [ (x : ls, rs)| (ls, rs) <- combines (pred n) xs]
        ds = [ (ls, x : rs) | (ls, rs) <- combines n xs]


group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[ ]]
group (x : xs) ys = [ ls : gs | (ls, rs) <- combines x ys, gs <- group xs rs]

main = print $ group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
