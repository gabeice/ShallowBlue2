module Player where

import Square(Board,Square,Color(White,Black),Pos,Move(Move))
import Display(getMove)

data Player = Player PlayerType Color
data PlayerType = Human

color :: Player -> Color
color (Player _ c) = c

prompt :: Player -> Board -> [Move] -> Move
prompt (Player Human _) board ms = getMove (Display board) ms