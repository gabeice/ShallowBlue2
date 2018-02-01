module Piece where

data Piece = Piece { color :: Color, pieceType :: PieceType, position :: Pos } deriving Eq
data Color = Black | White deriving Eq
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq
type Pos = (Int, Int)
type Dir = (Int, Int)
type Step = (Int, Int)

symbol :: Piece -> Char
symbol (Piece _ King _) = '♚'
symbol (Piece _ Queen _) = '♛'
symbol (Piece _ Rook _) = '♜'
symbol (Piece _ Bishop _) = '♝'
symbol (Piece _ Knight _) = '♞'
symbol (Piece _ Pawn _) = '♟'

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

isSliding :: PieceType -> Bool
isSliding King = False
isSliding Queen = True
isSliding Rook = True
isSliding Bishop = True
isSliding Knight = False
isSliding Pawn = False

moveDirs :: PieceType -> [Dir]
moveDirs Queen = [(-1,-1),(-1,0),(-1,1),(0,1),(0,-1),(1,-1),(1,0),(1,1)]
moveDirs Rook = [(-1,0),(0,-1),(0,1),(1,0)]
moveDirs Bishop = [(-1,-1),(-1,1),(1,-1),(1,1)]

moveSteps :: PieceType -> [Step]
moveSteps King = [(-1,-1),(-1,0),(-1,1),(0,1),(0,-1),(1,-1),(1,0),(1,1)]
moveSteps Knight = [(-1,-2),(-1,2),(-2,1),(-2,-1),(1,2),(2,1),(1,-2),(2,-1)]