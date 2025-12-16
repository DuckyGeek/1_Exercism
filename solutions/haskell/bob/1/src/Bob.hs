module Bob (responseFor) where

import Data.Char (isUpper, isAlpha, isSpace)
import Data.List (all, dropWhileEnd)

isYelling :: String -> Bool
isYelling xs = any isAlpha xs && all (\c -> not (isAlpha c) || isUpper c) xs

responseFor :: String -> String
responseFor xs
  | null xs || all isSpace xs = "Fine. Be that way!"
  | isYelling xs && not (null xs) && last xs == '?' = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | not (null trimmed) && last trimmed == '?' = "Sure."
  | otherwise = "Whatever."
  where
    trimmed = dropWhileEnd isSpace xs
