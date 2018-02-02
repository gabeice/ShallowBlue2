module Display where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Board(Board)
import Move(Move)
import Piece(Pos,symbol)

data Display = Display { board :: Board }

initializeDisplay :: IO Window
initializeDisplay = do initCurses
                       initPair (Pair 1) (Color 60) (Color 233)
                       initPair (Pair 2) (Color 233) (Color 60)
                       screen <- initScr
                       keypad screen True
                       echo False
                       cBreak True
                       return screen

led = convertAttributes[Reverse]

clearDisplay :: Window -> IO ()
clearDisplay w = do keypad w False
                    cBreak False
                    echo True
                    endWin

--render (Display board) (a,b) = do [mvWAddStr w 1 2 "K" | w <- renderSquares]
--                                  [wRefresh w | w <- renderSquares]

initColors = do initPair (Pair 1) black (Color 94)
                initPair (Pair 2) black (Color 101)
                initPair (Pair 3) white (Color 94)
                initPair (Pair 4) white (Color 101)

renderSquares :: Window -> [(Int,Int)] -> IO Window
renderSquares screen [a] = renderSquare a 'O' screen
renderSquares screen (x:xs) = do renderSquare x 'O' screen
                                 renderSquares screen xs

renderSquare :: (Int,Int) -> Char -> Window -> IO Window
renderSquare (i,j) symbol screen = do win <- newWin 3 6 (i * 3) (j * 6)
                                      if mod (i + j) 2 == 0
                                      then wAttrSet win (attr0, (Pair 1))
                                      else wAttrSet win (attr0, (Pair 2))
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

test = do initCurses
          initPair (Pair 1) black (Color 94)
          initPair (Pair 2) black (Color 101)
          screen <- initScr
          keypad screen True
          echo False
          cBreak True
          renderSquares screen [(i,j) | i <- [0..7], j <- [0..7]]
          refresh
          wait 7000000
          endWin