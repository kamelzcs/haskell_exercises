module Palindromes (largestPalindrome, smallestPalindrome) where

---- the algorithm is slow now...

import Control.Monad.State.Lazy

-- It's ok to return duplicates in the factor list, and the order of the factors
-- is irrelevant.
--
-- You should consider using a slightly different algorithm to find small or
-- large palindromes.

-- largestPalindrome minFactor maxFactor = (value, [(factor1, factor2)])

isPali :: Integral a => a -> Bool
isPali v = let s = (show $ toInteger v) in (s == reverse s)

update :: Integral a => (a -> a -> Bool) -> a -> a -> State a ()
update f x y = if isPali v then modify (\s -> if (f v s) then v else s) else return ()
  where v = x * y

largestPalindrome :: Integral a => a -> a -> (a, [(a, a)])
largestPalindrome = palindrome (>)

palindrome f minFactor maxFactor = factorize $ execState (sequence [s | l <- [minFactor..maxFactor], r <- [l .. maxFactor], let s = (update f) l r]) defaultV
  where factorize v = (v, [(l, v') | l <- [minFactor .. maxFactor], let v' = v `div`l,  v `mod` l == 0, v' <= maxFactor, sameLength l v'])
        defaultV = if (f minFactor maxFactor) then (maxFactor ^ 2) else (minFactor ^ 2)
        sameLength l r = (==) (length $ show $ toInteger l) (length $ show $ toInteger r)


-- smallestPalindrome minFactor maxFactor = (value, [(factor1, factor2)])
smallestPalindrome :: Integral a => a -> a -> (a, [(a, a)])
smallestPalindrome = palindrome (<)
