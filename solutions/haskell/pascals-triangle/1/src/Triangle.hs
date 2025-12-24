module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x 
  | x <= 0    = []
  | otherwise = take x (iterate nextRow [1])

nextRow :: [Integer] -> [Integer]
nextRow current = 
    let 
        leftShift  = [0] ++ current
        rightShift = current ++ [0]
    in 
        zipWith (+) leftShift rightShift