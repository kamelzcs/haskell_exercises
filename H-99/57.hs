data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

add :: Ord a => a -> Tree a -> Tree a
add x Empty            = Branch x Empty Empty
add x t@(Branch y l r)
  | x < y = Branch y (add x l) r
  | x > y = Branch y l (add x r)
  | otherwise = t
 
construct xs = foldl (flip add) Empty xs
