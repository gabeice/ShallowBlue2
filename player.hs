module Player where

import Square(Board,Square,Color(White,Black),Pos,Move(Move))

data Player = Player1 | Player2

color :: Player -> Color
color Player1 = White
color Player2 = Black

targetPiece :: Move -> Square
targetPiece (Move p _) = p

toPos :: Move -> Pos
toPos (Move _ p) = p

prompt :: Player -> [Move] -> Move
prompt p ms = head ms