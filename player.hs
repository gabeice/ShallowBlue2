module Player where

import Piece(Piece(Piece),Color,PieceType(Rook),Pos,position,pieceType,color)
import Move(Move(Move),executeMove,toPos,pawnTwoStep,move)
import Board(Board,startBoard,validMoves,king,isChecked,isMated,notOccupied)
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

availableMoves :: Color -> Log -> [Move]
availableMoves col log = (validMoves col board) ++ (specialMoves col log)
    where board = currentBoard log

specialMoves :: Color -> Log -> [Move]
specialMoves col log = (castles col log) ++ (enPassant col log)

castles :: Color -> Log -> [Move]
castles col log = [(Move k p) | p <- twoTowards]
    where twoTowards = map (\p -> towards 2 kingPos p) rookPositions
          rookPositions = map position rooks
          rooks = filter (\p -> (pieceType p) == Rook && (Piece.color p) == col && canCastle k p log) board
          board = currentBoard log
          k = head (king col board)
          kingPos = position k

hasMoved :: Piece -> Log -> Bool
hasMoved (Piece _ _ pos) log = any (\m -> (toPos m) == pos) log

towards :: Int -> Pos -> Pos -> Pos
towards n (x,y1) (_,y2) = (x, if y1 < y2 then y1 + n else y1 - n)

canCastle king rook log = unmoved king && unmoved rook && all empty inBetweens && all safe inBetweens
    where inBetweens = [oneTowards kingPos rookPos, twoTowards kingPos rookPos]
          kingPos = position king
          rookPos = position rook
          empty = (\p -> notOccupied p board)
          safe = (\p -> not (isChecked (Piece.color king) (move king p board)))
          board = currentBoard log
          oneTowards = towards 1
          twoTowards = towards 2
          unmoved = (\p -> not (hasMoved p log))

enPassant :: Color -> Log -> [Move]
enPassant _ [] = []
enPassant col log = if pawnTwoStep lastMove && (not (null adj))
                    then [(Move (head adj) (intermediary lastMove))]
                    else []
    where board = currentBoard log
          lastMove = head log
          adj = findAdjacent (toPos lastMove) col board

findAdjacent :: Pos -> Color -> Board -> [Piece]
findAdjacent pos col board = filter (\p -> (oneBeside (position p) pos) && (Piece.color p) == col) board

oneBeside :: Pos -> Pos -> Bool
oneBeside (x1,y1) (x2,y2) = x1 == x2 && (abs (y1 - y2)) == 1

intermediary :: Move -> Pos
intermediary (Move (Piece _ _ (x1,y)) (x2,_)) = ((x1 + x2) `div` 2, y)

prompt :: Player -> Log -> IO Move
prompt (Player Human col) log = getMove (Display board (lastMove log) moves)
    where board = currentBoard log
          moves = availableMoves col log