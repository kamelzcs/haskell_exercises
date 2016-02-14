
gray :: Int -> [String]
gray 0 = [""]
gray n = ['0' : p | p <- nxt] ++ ['1' : p | p <- reverse nxt]
  where nxt = gray (pred n)

main = print $ gray 3
