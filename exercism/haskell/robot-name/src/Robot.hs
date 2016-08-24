module Robot (Robot, mkRobot, resetName, robotName) where

-- The task is to create the data type `Robot`, as a
-- mutable variable, and implement the functions below.

import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import System.Random (randomRIO)
import Control.Monad

data Robot = Robot {getContent:: IORef String}

makeName :: IO String
makeName = mapM randomRIO pattern where
  pattern = [letter, letter, number, number, number]
  letter = ('A', 'Z')
  number = ('0', '9')

mkRobot :: IO Robot
mkRobot = makeName >>= newIORef >>= return . Robot

resetName :: Robot -> IO ()
resetName r = makeName >>= \s -> modifyIORef (getContent r) (const s)

robotName :: Robot -> IO String
robotName = readIORef . getContent
