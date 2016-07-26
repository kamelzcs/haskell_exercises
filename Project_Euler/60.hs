
primes = 2 : filter isPrime [3, 5..]
isPrime :: Integer -> Bool
isPrime = null . tail . primeFactor primes
primeFactor p@(a : as) n
  | a * a > n = [n]
  | n `mod` a == 0 = a : primeFactor p (n `div` a)
  | otherwise = primeFactor as n

solve = do
  a <- primes10000
  let b' = coPrime a primes10000
  b <- b'
  let c' = coPrime b b'
  c <- c'
  let d' = coPrime c c'
  d <- d'
  let e' = coPrime d d'
  e <- e'
  return [a, b, c, d, e]
  where coPrime x ys = [y | y <- ys, y > x, f x y]
        f x y = (isPrime $ read $ show x ++ show y) && (isPrime $ read $ show y ++ show x)
        primes10000 = takeWhile (<10000) primes



main = print $ sum $ head $ solve
