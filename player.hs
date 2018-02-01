module Player where

import Square(Board,Color,Move,move,targetPiece,toPos)
import Board(startBoard,validMoves,isMated)
import Display(Display,getMove)

data Player = Player PlayerType Color
data PlayerType = Human | AI
type Log = [Move]

color :: Player -> Color
color (Player _ c) = c

currentBoard :: Log -> Board
currentBoard [] = startBoard
currentBoard (m:ms) = (move (targetPiece m) (toPos m) lastBoard)
    where lastBoard = currentBoard ms

hasLost :: Color -> Log -> Bool
hasLost col log = isMated col (currentBoard log)

prompt :: Player -> Log -> Move
prompt (Player Human col) log = getMove (Display board) ms
    where board = currentBoard log
          ms = validMoves col board