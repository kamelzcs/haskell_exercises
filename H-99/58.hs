data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree x = let (q, r) = (x - 1) `quotRem` 2
                 in [Branch 'a' left right |
                     i <- [q..q + r],
                     left <- cbalTree i,
                     right <- cbalTree (x - 1 - i)]

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n
  | even n = []
  | otherwise = [ Branch 'x' t (reverseTree t) | t <- cbalTree (n `div` 2)]


reverseTree Empty = Empty
reverseTree (Branch x l r) = Branch x (reverseTree r) (reverseTree l)
