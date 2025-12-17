module Yacht (yacht, Category(..)) where
import Data.List (group, sort)
data Category = Ones | Twos | Threes | Fours | Fives | Sixes | FullHouse | FourOfAKind | LittleStraight | BigStraight | Choice | Yacht
yacht :: Category -> [Int] -> Int
yacht Ones dice = numberScore 1 dice
yacht Twos dice = numberScore 2 dice
yacht Threes dice = numberScore 3 dice
yacht Fours dice = numberScore 4 dice
yacht Fives dice = numberScore 5 dice
yacht Sixes dice = numberScore 6 dice
yacht FullHouse dice =
  if all ((`elem` [2, 3]) . length) $ group $ sort $ dice then sum dice else 0
yacht FourOfAKind dice =
  sum $ concat $ filter ((== 4) . length) $ map (take 4) $ group $ sort dice
yacht LittleStraight dice = if (== [1..5]) $ sort $ dice then 30 else 0
yacht BigStraight dice = if (== [2..6]) $ sort $ dice then 30 else 0
yacht Choice dice = sum dice
yacht Yacht dice = if (== 1) $ length $ group $ dice then 50 else 0
numberScore :: Int -> [Int] -> Int
numberScore n = (* n) . length . filter (== n)