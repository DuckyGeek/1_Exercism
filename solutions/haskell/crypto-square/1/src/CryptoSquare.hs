module CryptoSquare (encode) where

import Data.Char (toLower, isAlphaNum)
import Data.List (transpose)

encode :: String -> String
encode rawText = 
    if null normalizedText 
    then "" 
    else unwords codeRows 
  where
    normalizedText = filter isAlphaNum (map toLower rawText)

    len = length normalizedText
    c = ceiling (sqrt (fromIntegral len))
    r = if c * (c - 1) >= len then c - 1 else c
    totalSlots = r * c
    spacesNeeded = totalSlots - len
    paddedText = normalizedText ++ replicate spacesNeeded ' '
    rows = chunks c paddedText
    codeRows = transpose rows

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)