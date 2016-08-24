module Deque (Deque, mkDeque, pop, push, shift, unshift) where

-- The task is to create the data type `Deque`
-- and implement the functions below.

import Data.IORef

data DList a = 
data Deque a = Deque (IORef (DList a))

mkDeque :: IO (Deque a)
mkDeque = fmap Deque $ newIORef []

pop :: Deque a -> IO (Maybe a)
pop = undefined

push :: Deque a -> a -> IO ()
push = undefined

unshift :: Deque a -> a -> IO ()
unshift = undefined

shift :: Deque a -> IO (Maybe a)
shift = undefined
