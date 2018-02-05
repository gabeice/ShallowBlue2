module Player where

import Piece(Color,Pos)
import Move(Move,executeMove,toPos)
import Board(Board,startBoard,validMoves,isMated)
import Display(Display(Display),getMove)

data Player = Player { playerType :: PlayerType, color :: Color }
data PlayerType = Human | AI
type Log = [Move]

currentBoard :: Log -> Board
currentBoard [] = startBoard
currentBoard (m:ms) = (executeMove m (currentBoard ms))

lastMove :: Log -> Pos
lastMove [] = (0,0)
lastMove log = (toPos (head log))

hasLost :: Color -> Log -> Bool
hasLost col log = isMated col (currentBoard log)

prompt :: Player -> Log -> IO Move
prompt (Player Human col) log = getMove (Display board (lastMove log) moves)
    where board = currentBoard log
          moves = validMoves col board