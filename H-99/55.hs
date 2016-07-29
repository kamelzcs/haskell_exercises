data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree x = let (q, r) = (x - 1) `quotRem` 2
                 in [Branch 'a' left right |
                     i <- [q..q + r],
                     left <- cbalTree i,
                     right <- cbalTree (x - 1 - i)]
