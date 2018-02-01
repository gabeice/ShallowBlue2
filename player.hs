module Player where

import Square(Board,Square,Color(White,Black),Pos,Move(Move))

data Player = Player1 | Player2

color :: Player -> Color
color Player1 = White
color Player2 = Black

prompt :: Player -> [Move] -> Move
PROMPT ALL THE THINGS!!!!!!!!!!!!