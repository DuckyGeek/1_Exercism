module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick [] = []
tick grid =
  let
    h = length grid
    w = length (head grid)

    isAlive :: Int -> Int -> Bool
    isAlive r c
      | r < 0 || r >= h || c < 0 || c >= w = False 
      | otherwise = (grid !! r !! c) == 1         

    countNeighbors :: Int -> Int -> Int
    countNeighbors r c = length 
      [ () | dr <- [-1, 0, 1],       
             dc <- [-1, 0, 1],      
             (dr, dc) /= (0, 0),     
             isAlive (r + dr) (c + dc) ] 

    nextState :: Int -> Int -> Int
    nextState r c = 
      let 
        aliveNow = isAlive r c      
        neighbors = countNeighbors r c 
      in 
        if aliveNow 
        then 
           if neighbors == 2 || neighbors == 3 then 1 else 0
        else 
           if neighbors == 3 then 1 else 0

  in
    [ [ nextState r c | c <- [0 .. w-1] ] | r <- [0 .. h-1] ]