module Robot
    ( Bearing (..)
    , Robot
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

import Data.List

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Robot = Robot { bear :: Bearing,
                     pos :: (Integer, Integer)} deriving (Eq, Show)

-- The task is to create the data type `Robot`, with `Eq`
-- and `Show` instances, and implement the functions below.

bearing :: Robot -> Bearing
bearing = bear

coordinates :: Robot -> (Integer, Integer)
coordinates = pos

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

toDir :: Bearing -> (Integer->Integer, Integer->Integer)
toDir North = (id, (+1))
toDir East = ((+1), id)
toDir South = (id, subtract 1)
toDir West = (subtract 1, id)

applyTwo (f, g) (x, y) = (f x, g y)


simulate :: Robot -> String -> Robot
simulate b = foldl' op b
  where op (Robot bear pos) c | c == 'L' = Robot (turnLeft bear) pos
                              | c == 'R' = Robot (turnRight bear) pos
                              | c == 'A' = Robot bear $ applyTwo (toDir bear) pos

turnLeft :: Bearing -> Bearing
turnLeft b = toEnum $ ((+ 3) $ fromEnum b) `mod` 4

fpow n f x = iterate f x !! n

turnRight :: Bearing -> Bearing
turnRight b = fpow 3 turnLeft b
