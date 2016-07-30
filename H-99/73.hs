import Data.List

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

lisp :: Tree Char -> String
lisp (Node x []) = [x]
lisp (Node x xs) = "(" ++ [x] ++ (concatMap lisp xs) ++ ")"
