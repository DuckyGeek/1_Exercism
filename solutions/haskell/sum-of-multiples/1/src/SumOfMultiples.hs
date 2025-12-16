module SumOfMultiples (sumOfMultiples) where

import Data.List (subsequences)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let
    validFactors = filter (> 0) factors
    subsets = tail (subsequences validFactors)  
    total = sum (map (\subset -> let
                                  lcmVal = foldl lcm 1 subset
                                  count = (limit - 1) `div` lcmVal
                                  sumSubset = lcmVal * count * (count + 1) `div` 2
                                in
                                  if length subset `mod` 2 == 1 then sumSubset else -sumSubset
                            ) subsets)
  in
    total