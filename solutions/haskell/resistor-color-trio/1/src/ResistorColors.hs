module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

colorToValue :: Color -> Int
colorToValue Black  = 0
colorToValue Brown  = 1
colorToValue Red    = 2
colorToValue Orange = 3
colorToValue Yellow = 4
colorToValue Green  = 5
colorToValue Blue   = 6
colorToValue Violet = 7
colorToValue Grey   = 8
colorToValue White  = 9

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
    let mainValue = colorToValue c1 * 10 + colorToValue c2
        zeros = colorToValue c3
    in mainValue * (10 ^ zeros)

label :: Resistor -> String
label resistor =
    let value = ohms resistor
        (num, unit) = case value of
            v | v < 1000      -> (v, "ohms")
              | v < 1000000   -> (v `div` 1000, "kiloohms")
              | v < 1000000000 -> (v `div` 1000000, "megaohms")
              | otherwise     -> (v `div` 1000000000, "gigaohms")
    in show num ++ " " ++ unit