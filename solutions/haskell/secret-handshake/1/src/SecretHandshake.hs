module SecretHandshake (handshake) where

handshakeAcc :: [String] -> Int -> [String]
handshakeAcc acc i
    | n >= 16 = reverse (handshakeAcc acc (n-16))
    | n >= 8 = handshakeAcc ("jump":acc) (n-8)
    | n >= 4 = handshakeAcc ("close your eyes":acc) (n-4)
    | n >= 2 = handshakeAcc ("double blink":acc) (n-2)
    | n == 1 = handshakeAcc ("wink":acc) (n-1)
    | n == 0 = acc
    where n = i `mod` 32


handshake :: Int -> [String]
handshake = handshakeAcc []