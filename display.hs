module Display where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Control.Concurrent

import Board(Board,startBoard)
import Move(Move(Move),toPos,getPos,diff,offBoard,targetPiece)
import Piece(Piece,Pos,Color(White,Black),Dir,symbol,color,position)

data Display = Display { board :: Board, startPos :: Pos, moveList :: [Move] }

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

render :: Board -> Pos -> IO ()
render board cursorPos = render' board cursorPos [(i,j) | i <- [0..7], j <- [0..7]]

render' :: Board -> Pos -> [Pos] -> IO ()
render' board cursorPos [a] = renderSquare a (getAttrs board a) cursorPos
render' board cursorPos (x:xs) = do renderSquare x (getAttrs board x) cursorPos
                                    render' board cursorPos xs

renderSquare :: Pos -> (Char, Piece.Color) -> Pos -> IO ()
renderSquare (i,j) (symbol, color) cursorPos = do win <- newWin 3 6 (i * 3) (j * 6)
                                                  if (i,j) == cursorPos
                                                  then if color == Black
                                                       then wAttrSet win (attr0, (Pair 5))
                                                       else wAttrSet win (attr0, (Pair 7))
                                                  else if mod (i + j) 2 == 0
                                                       then if color == Black then wAttrSet win (attr0, (Pair 1))
                                                                              else wAttrSet win (attr0, (Pair 3))
                                                       else if color == Black then wAttrSet win (attr0, (Pair 2))
                                                                              else wAttrSet win (attr0, (Pair 4))
                                                  mvWAddStr win 1 2 ("\b " ++ (symbol : "   "))
                                                  wBorder win (Border ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ')
                                                  wRefresh win

getFromPos :: Display -> Pos -> IO Piece
getFromPos display startPos = do render board startPos
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

getToPos :: Display -> Piece -> Pos -> IO Pos
getToPos display fromPos startPos = do render board startPos
                                       x <- getch
                                       if elem (toInteger x) navigationKeys
                                       then repeat (newPos startPos (toInteger x))
                                       else if x == returnKey
                                            then if not (elem (Move fromPos startPos) (moveList display))
                                                 then repeat startPos
                                                 else return startPos
                                            else repeat startPos
    where board = (Display.board display)
          repeat = getToPos display fromPos

getMove :: Display -> IO Move
getMove display = do pieceToMove <- getFromPos display (startPos display)
                     toPos <- getToPos display pieceToMove (position pieceToMove)
                     return (Move pieceToMove toPos)

test = do screen <- initializeDisplay
          mv <- getFromPos (Display startBoard (0,0) []) (0,0)
          wAddStr screen [symbol mv]
          refresh
          threadDelay 1000000
          clearDisplay screen