module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0      = Nothing
  | aliquot > n = Just Abundant
  | aliquot < n = Just Deficient
  | otherwise   = Just Perfect
  where aliquot = sum [x | x <- [1..(n `div` 2)], n `mod` x == 0]