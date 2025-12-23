module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    targetLower = map toLower target
    targetSorted = sort targetLower

    isAnagram :: String -> Bool
    isAnagram candidate =
        let candidateLower = map toLower candidate
            candidateSorted = sort candidateLower
        in 
            candidateLower /= targetLower && 
            candidateSorted == targetSorted