module Game where

import Board
import Square(Square,Pos)

type Game = [Move]
data Move = Move Square Pos

currentBoard :: Game -> Board

play :: Game