{-# LANGUAGE ViewPatterns #-}
module IsbnVerifier (isbn) where
import Text.Read (readMaybe)
isbn :: String -> Bool
isbn (filter (/= '-') -> s)
    | length s == 10
    , Just xs <- mapM (readMaybe . pure) (init s)
    , Just c <- if last s == 'X' then Just 10 else readMaybe [last s]
    , sum (zipWith (*) (xs ++ [c]) [10, 9..]) `mod` 11 == 0 = True
    | otherwise = False