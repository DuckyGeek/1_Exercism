module Robot (Bearing(..), bearing, coordinates, mkRobot, move) where

data Bearing = North | East | South | West deriving (Eq, Show)
data Robot = Robot Bearing (Integer, Integer) deriving (Eq,Show)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ c) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot [] = robot
move robot (c:cs) = move (step robot c) cs
  where
    step :: Robot -> Char -> Robot
    step (Robot North (x, y)) 'R' = Robot East (x, y)
    step (Robot East (x, y)) 'R' = Robot South (x, y)
    step (Robot South (x, y)) 'R' = Robot West (x, y)
    step (Robot West (x, y)) 'R' = Robot North (x, y)
    step (Robot North (x, y)) 'L' = Robot West (x, y)
    step (Robot West (x, y)) 'L' = Robot South (x, y)
    step (Robot South (x, y)) 'L' = Robot East (x, y)
    step (Robot East (x, y)) 'L' = Robot North (x, y)
    step (Robot North (x, y)) 'A' = Robot North (x, y+1)
    step (Robot East (x, y)) 'A' = Robot East (x+1, y)
    step (Robot South (x, y)) 'A' = Robot South (x, y-1)
    step (Robot West (x, y)) 'A' = Robot West (x-1, y)
    step robot _ = robot