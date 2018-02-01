module Player where

import Square(Board,Square,Color(White,Black),Pos,Move(Move))

data Player = Player PlayerType Color
data PlayerType = Human

color :: Player -> Color
color (Player _ c) = c

prompt :: Player -> [Move] -> Move
PROMPT ALL THE THINGS!!!!!!!!