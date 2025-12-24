module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate [] = [] 
annotate board =
  let
    h = length board
    w = length (head board)

    isMine :: Int -> Int -> Bool
    isMine r c
      | r < 0 || r >= h || c < 0 || c >= w = False 
      | otherwise = (board !! r !! c) == '*'

    countMines :: Int -> Int -> Int
    countMines r c = length 
      [ () | dr <- [-1, 0, 1],  
             dc <- [-1, 0, 1],  
             (dr, dc) /= (0, 0), 
             isMine (r + dr) (c + dc) ] 

    getCell :: Int -> Int -> Char
    getCell r c
      | isMine r c = '*'        
      | count == 0 = ' '       
      | otherwise  = intToDigit count
      where count = countMines r c

  in
    [ [ getCell r c | c <- [0 .. w-1] ] | r <- [0 .. h-1] ]