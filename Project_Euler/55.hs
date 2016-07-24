reverseNum = read . reverse . show
palindrome x = x == reverseNum x
lychel = not . any palindrome . take 50 . tail . iterate nxt
  where nxt x = x + reverseNum x
main :: IO ()
main = print $ length $ filter lychel [1..10000]
