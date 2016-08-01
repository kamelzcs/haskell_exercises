data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

mirror Empty          Empty          = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _              _              = False

symmetric Empty          = True
symmetric (Branch _ l r) = mirror l r
