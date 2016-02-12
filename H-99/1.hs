myLast :: [a] -> a
myLast [] = error "I am empty"
myLast [x] = x
myLast (x : xs) = myLast xs

