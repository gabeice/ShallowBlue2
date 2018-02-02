module Display where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Control.Concurrent

import Board(Board,startBoard)
import Move(Move,getPos)
import Piece(Pos,Color(White,Black),symbol,color)

data Display = Display { board :: Board, startPos :: Pos, moveList :: [Move] }

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

--printMessage :: Display -> String -> IO ()
--
--getFromPos :: Display -> Pos -> IO Piece
--getFromPos display startPos = do render (board display) startPos
--                                 x <- getch
--                                 | x ==
--
--getToPos :: Display -> Pos -> [Move] -> IO Pos
--
--getMove :: Display -> IO Move
--getMove display = do fromPos <- getFromPos display (startPos display)
--                     toPos <- getToPos display (position fromPos)
--                     return (Move fromPos toPos)

test = do screen <- initializeDisplay
          render startBoard (0,5)
          refresh
          threadDelay 1000000
          clearDisplay screen