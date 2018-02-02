module Display where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Board(Board,startBoard)
import Move(Move,getPos)
import Piece(Pos,Color(White,Black),symbol,color)

data Display = Display { board :: Board }

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

getAttrs :: Board -> Pos -> (Char, Piece.Color)
getAttrs board pos = if null piece
                     then (' ', White)
                     else (symbol (piece !! 0), Piece.color (piece !! 0))
    where piece = getPos pos board

render :: Window -> Board -> [Pos] -> IO Window
render screen board [a] = renderSquare a (getAttrs board a) screen
render screen board (x:xs) = do renderSquare x (getAttrs board x) screen
                                render screen board xs

renderSquare :: Pos -> (Char, Piece.Color) -> Window -> IO Window
renderSquare (i,j) (symbol, color) screen = do win <- newWin 3 6 (i * 3) (j * 6)
                                               if mod (i + j) 2 == 0
                                               then if color == Black then wAttrSet win (attr0, (Pair 1))
                                                                      else wAttrSet win (attr0, (Pair 3))
                                               else if color == Black then wAttrSet win (attr0, (Pair 2))
                                                                      else wAttrSet win (attr0, (Pair 4))
                                               mvWAddStr win 1 2 ("\b " ++ (symbol : "   "))
                                               wBorder win (Border ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ')
                                               wRefresh win
                                               return screen

--printMessage :: Display -> String -> IO ()
--
--getFromPos :: Display -> Pos -> [Move] -> IO Piece
--getFromPos display startPos ms = do render display startPos
--                                    x <- getCh
--                                    | x ==
--
--getToPos :: Display -> Pos -> [Move] -> IO Pos
--
--getMove :: Display -> Pos -> [Move] -> IO Move
--getMove display startPos ms = do fromPos <- getFromPos display startPos ms
--                                 toPos <- getToPos display (position fromPos) ms
--                                 return (Move fromPos toPos)

wait n = sequence_ [return () | _ <- [1..n]]

test = do screen <- initializeDisplay
          render screen startBoard [(i,j) | i <- [0..7], j <- [0..7]]
          refresh
          wait 7000000
          clearDisplay screen