module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys 
  | length xs /= length ys = Nothing
  | otherwise = Just (countDifferences xs ys)
  where
    countDifferences :: String -> String -> Int
    countDifferences a b = length (filter (\(x,y) -> x /= y)(zip a b))
