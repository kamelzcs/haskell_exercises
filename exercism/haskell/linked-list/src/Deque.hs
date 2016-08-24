module Deque (Deque, mkDeque, pop, push, shift, unshift) where

-- The task is to create the data type `Deque`
-- and implement the functions below.

import Data.IORef

data Node a = Nil | Node {value :: IORef a, prev :: IORef (Node a), next :: IORef (Node a)} deriving Eq
data Deque a = Deque {first:: IORef (Node a), last :: IORef (Node a)}

mkDeque :: IO (Deque a)
mkDeque = let tail = Nil
          in  Deque <$> (newIORef tail) <*> (newIORef tail)

pop :: Deque a -> IO (Maybe a)
pop (Deque firstRef lastRef) = do (Node valueRef prevRef nextRef) <- readIORef lastRef
                                  prevNode <- readIORef prevRef
                                  writeIORef lastRef prevNode
                                  value <- readIORef valueRef
                                  return $ return value

unit :: a -> IO (Node a)
unit v = do let dummy = Nil
            Node <$> newIORef v <*> newIORef dummy <*> newIORef dummy

push :: Deque a -> a -> IO ()
push (Deque firstRef lastRef) v = do node <- readIORef lastRef
                                     if (node == Nil) then do
                                       newNode <- unit v
                                       writeIORef firstRef newNode
                                       writeIORef lastRef newNode
                                       return ()
                                     else
                                       do
                                       let valueRef = value node
                                           prevRef = prev node
                                           nextRef = next node
                                       newNode <- unit v
                                       writeIORef nextRef newNode
                                       writeIORef (prev newNode) node
                                       writeIORef lastRef newNode
                                       return ()

unshift :: Deque a -> a -> IO ()
unshift (Deque firstRef lastRef) v = do node <- readIORef firstRef
                                        if (node == Nil) then do
                                          newNode <- unit v
                                          writeIORef firstRef newNode
                                          writeIORef lastRef newNode
                                          return ()
                                        else
                                          do
                                           let valueRef = value node
                                               prevRef = prev node
                                               nextRef = next node
                                           newNode <- unit v
                                           writeIORef prevRef newNode
                                           writeIORef (next newNode) node
                                           writeIORef firstRef newNode
                                           return ()

shift :: Deque a -> IO (Maybe a)
shift (Deque firstRef lastRef) = do (Node valueRef prevRef nextRef) <- readIORef firstRef
                                    nextNode <- readIORef nextRef
                                    writeIORef firstRef nextNode
                                    value <- readIORef valueRef
                                    return $ return value
