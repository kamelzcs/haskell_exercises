compress [] = []
compress [x] = [x]
compress ds = foldl (\r a -> if (last r == a) then r else r ++ [a]) [head ds] ds
