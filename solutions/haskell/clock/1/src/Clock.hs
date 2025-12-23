module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min =
  let
    minutesInDay = 1440
    totalMinutes = (hour * 60) + min
    result = totalMinutes `mod` minutesInDay
  in
    Clock result

toString :: Clock -> String
toString (Clock minutes) =
  let
    h = minutes `div` 60
    m = minutes `mod` 60
    pad number = if number < 10 
                 then "0" ++ show number 
                 else show number
  in
    pad h ++ ":" ++ pad m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock currentMinutes) =
  let
    minutesToAdd = (hour * 60) + min
    newTotal = currentMinutes + minutesToAdd
  in
    fromHourMin 0 newTotal