split xs n = split' n "" xs

split' 0 as xs = (as, xs)
split' n as (x : xs) = split' (pred n) (as ++ [x]) xs

main = print $ split "abcdefghik" 3
