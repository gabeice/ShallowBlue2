module Player where

import Piece(Color)
import Move(Move,move,targetPiece,toPos)
import Board(Board,startBoard,validMoves,isMated)
import Display(Display,getMove)

data Player = Player { playerType :: PlayerType, color :: Color }
data PlayerType = Human | AI
type Log = [Move]

currentBoard :: Log -> Board
currentBoard [] = startBoard
currentBoard (m:ms) = (move (targetPiece m) (toPos m) lastBoard)
    where lastBoard = currentBoard ms

lastMove :: Log -> Pos
lastMove [] = (0,0)
lastMove log = (toPos (head log))

hasLost :: Color -> Log -> Bool
hasLost col log = isMated col (currentBoard log)

prompt :: Player -> Log -> Move
prompt (Player Human col) log = getMove (Display board (lastMove log) ms)
    where board = currentBoard log
          ms = validMoves col board