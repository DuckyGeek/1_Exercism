module RunLength (decode, encode) where
import Data.Char
decode :: String -> String
decode [] = []
decode xs
    | null eqs = decodeStrip difs 1
    | otherwise = decodeStrip difs (read eqs::Int)
    where (eqs, difs) = span isNumber xs
          decodeStrip (y:ys) n = (replicate n y)++(decode ys)
encode :: String -> String
encode [] = []
encode (x:xs) = (encodeStrip eqs) ++ (encode difs)
    where (eqs, difs) = span (== x) xs
          encodeStrip [] = [x]
          encodeStrip ys = show ((length ys) + 1) ++ [x]
