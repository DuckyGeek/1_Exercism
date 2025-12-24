module Queens (boardString, canAttack) where

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2)
  | r1 == r2 = True
  | c1 == c2 = True
  | abs (r1 - r2) == abs (c1 - c2) = True
  | otherwise = False

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = 
  unlines [ unwords [ render r c | c <- [0..7] ] | r <- [0..7] ]
  where
    render r c
      | Just (r, c) == white = "W"
      | Just (r, c) == black = "B"
      | otherwise            = "_"