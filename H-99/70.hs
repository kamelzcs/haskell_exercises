import Control.Monad.State

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

parses :: State String [Tree Char]
parses = do
  str <- get
  let c = head str
  case c of
    '^' -> do
        put (tail str)
        return []
    _ -> do
        let (t, s) = runState parse str
            (ts, s1) = runState parses s
        put s1
        return $ t : ts

parse :: State String (Tree Char)
parse = do
  str <- get
  let c = head str
      (ts, s) = runState parses (tail str)
  put s
  return $ Node c ts

stringToTree :: String -> Tree Char
stringToTree = fst . runState parse

treeToString :: Tree Char -> String
treeToString (Node x []) = [x] ++ "^"
treeToString (Node x xs) = [x] ++ (concatMap treeToString xs) ++ "^"
