myButLast :: [a] -> a
myButLast [] = error "too few elements"
myButLast [x] = error "too few elements"
myButLast [x, _] = x
myButLast (x: xs) = myButLast xs
