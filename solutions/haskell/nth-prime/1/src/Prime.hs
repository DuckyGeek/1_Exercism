module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))

primes :: [Integer]
primes = filter isPrime [2..]

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2 .. (k - 1)], k `mod` x == 0 ]