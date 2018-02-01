module Game where

import Board(startBoard)
import Square(Board,Square,Pos,move)

type Game = [Move]
data Move = Move Square Pos

targetPiece :: Move -> Square
targetPiece (Move p _) = p

toPos :: Move -> Pos
toPos (Move _ p) = p

currentBoard :: Game -> Board
currentBoard [m] = (move (targetPiece m) (toPos m) startBoard)
currentBoard (m:ms) = (move (targetPiece m) (toPos m) lastBoard)
    where lastBoard = currentBoard ms