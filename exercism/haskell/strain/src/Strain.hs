module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard f
  = keep (not . f)

keep :: (a -> Bool) -> [a] -> [a]
keep f ds = foldr (\x y -> if (f x) then x : y else y) [] ds
