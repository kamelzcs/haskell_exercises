main = print $ length $ filter larger $ take 1000 $ iterate nxt (2, 3)
  where nxt (x, y) = (x + 2 * y, x + y)
        larger (x, y) = length(show x) > length(show y)
