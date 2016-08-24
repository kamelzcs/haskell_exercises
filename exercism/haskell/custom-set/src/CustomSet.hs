module CustomSet
  ( CustomSet
  , delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)
import qualified Data.List as List

data CustomSet a = CustomSet {content :: [a]}

instance Ord a => Eq (CustomSet a) where
  x == y = (List.sort $ content x) == (List.sort $ content y)

instance Show a => Show (CustomSet a) where
  show x = "fromList " ++ show (content x)

-- The task is to create the data type `CustomSet`, with `Eq`
-- and `Show` instances, and implement the functions below.

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete v set = CustomSet $ List.delete v (content set)

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference xs ys = CustomSet $ (content xs) List.\\ (content ys)

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList = CustomSet . List.nub

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert v set = CustomSet $ List.nub $ v : (content set)

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection xs ys = CustomSet $ List.intersect (content xs) (content ys)

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom xs ys = List.null $ List.intersect (content xs) (content ys)

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf xs ys = List.isSubsequenceOf (content xs) (content ys)

member :: Eq a => a -> CustomSet a -> Bool
member v set = List.elem v (content set)

null :: CustomSet a -> Bool
null = List.null . content

size :: CustomSet a -> Int
size = List.length . content

toList :: CustomSet a -> [a]
toList = content

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union xs ys = CustomSet $ List.union (content xs) (content ys)
