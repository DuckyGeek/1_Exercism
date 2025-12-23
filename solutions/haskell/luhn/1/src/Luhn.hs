module Luhn (isValid) where

import Data.Char (digitToInt, isDigit)

isValid :: String -> Bool
isValid n = 
    let 
        cleanStr = filter (/= ' ') n
        digits = map digitToInt cleanStr
    in 
        if length cleanStr <= 1 || not (all isDigit cleanStr)
        then False 
        else 
            sum (doubleEveryOther (reverse digits)) `mod` 10 == 0

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []         
doubleEveryOther [x] = [x]      
doubleEveryOther (x:y:zs) =    
    x : double y : doubleEveryOther zs
   
double :: Int -> Int
double x = 
    let doubled = x * 2
    in if doubled > 9 
       then doubled - 9 
       else doubled