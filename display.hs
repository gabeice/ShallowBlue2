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
                initPair (Pair 5) black red
                initPair (Pair 6) black red
                initPair (Pair 7) white green
                initPair (Pair 8) white green

getAttrs :: Board -> Pos -> (Char, Piece.Color)
getAttrs board pos = if null piece
                     then (' ', White)
                     else (symbol (head piece), Piece.color (head piece))
    where piece = getPos pos board

render :: Board -> IO ()
render board = render' board [(i,j) | i <- [0..7], j <- [0..7]]

render' :: Board -> [Pos] -> IO ()
render' board [a] = renderSquare a (getAttrs board a)
render' board (x:xs) = do renderSquare x (getAttrs board x)
                          render' board xs

renderSquare :: Pos -> (Char, Piece.Color) -> IO ()
renderSquare (i,j) (symbol, color) = do win <- newWin 3 6 (i * 3) (j * 6)
                                        if mod (i + j) 2 == 0
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
--getFromPos display startPos = do render display startPos
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
          render startBoard
          refresh
          threadDelay 1000000
          clearDisplay screen