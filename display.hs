module Display where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Control.Concurrent

import Board(Board,startBoard)
import Move(Move(Move),toPos,getPos,diff,offBoard,targetPiece)
import Piece(Piece,Pos,Color(White,Black),Dir,symbol,color,position)

data Display = Display { board :: Board, startPos :: Pos, moveList :: [Move] }
type Selection = (Pos,Pos)

downArrow = 258
upArrow = 259
leftArrow = 260
rightArrow = 261

returnKey = 10
quitKey = 113

navigationKeys :: [Integer]
navigationKeys = [downArrow, upArrow, leftArrow, rightArrow]

newPos :: Pos -> Integer -> Pos
newPos pos 258 = newPos' pos (1,0)
newPos pos 259 = newPos' pos (-1,0)
newPos pos 260 = newPos' pos (0,-1)
newPos pos 261 = newPos' pos (0,1)

newPos' :: Pos -> Dir -> Pos
newPos' pos dir = if offBoard new then pos else new
    where new = diff pos dir

initializeDisplay :: IO Window
initializeDisplay = do initCurses
                       initColors
                       screen <- initScr
                       keypad screen True
                       cursSet CursorInvisible
                       echo False
                       cBreak True
                       return screen

clearDisplay :: Window -> IO ()
clearDisplay w = do keypad w False
                    cBreak False
                    echo True
                    endWin

initColors :: IO ()
initColors = do initPair (Pair 1) black (Color 94)
                initPair (Pair 2) black (Color 101)
                initPair (Pair 3) white (Color 94)
                initPair (Pair 4) white (Color 101)
                initPair (Pair 5) black green
                initPair (Pair 6) black red
                initPair (Pair 7) white green
                initPair (Pair 8) white red

getAttrs :: Board -> Pos -> (Char, Piece.Color)
getAttrs board pos = if null piece
                     then (' ', White)
                     else (symbol (head piece), Piece.color (head piece))
    where piece = getPos pos board

setColor :: Window -> Int -> IO ()
setColor window pairNumber = wAttrSet window (attr0, (Pair pairNumber))

render :: Board -> Selection -> IO ()
render board selection = render' board selection [(i,j) | i <- [0..7], j <- [0..7]]

render' :: Board -> Selection -> [Pos] -> IO ()
render' board selection [a] = renderSquare a (getAttrs board a) selection
render' board selection (x:xs) = do renderSquare x (getAttrs board x) selection
                                    render' board selection xs

renderSquare :: Pos -> (Char, Piece.Color) -> Selection -> IO ()
renderSquare (i,j) (symbol, color) (cursorPos, selection) = do w <- newWin 3 6 (i * 3) (j * 6)
                                                               if (i,j) == selection
                                                               then if color == Black
                                                                    then setColor w 6
                                                                    else setColor w 8
                                                               else if (i,j) == cursorPos
                                                                    then if color == Black
                                                                         then setColor w 5
                                                                         else setColor w 7
                                                                    else if mod (i + j) 2 == 0
                                                                         then if color == Black then setColor w 1
                                                                                                else setColor w 3
                                                                         else if color == Black then setColor w 2
                                                                                                else setColor w 4
                                                               mvWAddStr w 1 2 ("\b " ++ (symbol : "   "))
                                                               wBorder w (Border ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ')
                                                               wRefresh w

getFromPos :: Display -> Pos -> IO Piece
getFromPos display startPos = do render board (startPos,(8,8))
                                 x <- getch
                                 if elem (toInteger x) navigationKeys
                                 then repeat (newPos startPos (toInteger x))
                                 else if x == returnKey
                                      then if null pieceAt || not (elem (head pieceAt) moveablePieces)
                                           then repeat startPos
                                           else return (head pieceAt)
                                      else repeat startPos
    where board = (Display.board display)
          moveablePieces = map targetPiece (moveList display)
          pieceAt = getPos startPos board
          repeat = getFromPos display

getToPos :: Display -> Piece -> Selection -> IO Pos
getToPos display fromPos (startPos,selection) = do render board (startPos,selection)
                                                   x <- getch
                                                   if elem (toInteger x) navigationKeys
                                                   then repeat (newPos startPos (toInteger x))
                                                   else if x == returnKey
                                                        then if elem (Move fromPos startPos) (moveList display) || startPos == (position fromPos)
                                                             then return startPos
                                                             else repeat startPos
                                                        else repeat startPos
    where board = (Display.board display)
          repeat = (\m -> getToPos display fromPos (m,selection))

getMove :: Display -> IO Move
getMove (Display b p l) = do pieceToMove <- getFromPos (Display b p l) p
                             toPos <- getToPos (Display b p l) pieceToMove ((position pieceToMove),(position pieceToMove))
                             if toPos == (position pieceToMove)
                             then getMove (Display b toPos l)
                             else return (Move pieceToMove toPos)