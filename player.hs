module Player where

import Square(Board,Square(Piece),PieceType(Pawn),Color(White,Black),Pos)

data Player = Player1 | Player2
data Move = Move Square Pos

color :: Player -> Color
color Player1 = White
color Player2 = Black

targetPiece :: Move -> Square
targetPiece (Move p _) = p

toPos :: Move -> Pos
toPos (Move _ p) = p

prompt :: Player -> [Board] -> Move