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

renderSquares :: Window -> [(Int,Int)] -> IO Window
renderSquares screen [a] = renderSquare a screen
renderSquares screen (x:xs) = do renderSquare x screen
                                 renderSquares screen xs

renderSquare :: (Int,Int) -> Window -> IO Window
renderSquare (i,j) screen = do win <- newWin 3 6 (i * 3) (j * 6)
                               if mod (i + j) 2 == 0 then wAttrSet win (led, (Pair 1)) else wAttrSet win (led, (Pair 2))
                               mvWAddStr win 1 2 "K"
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
          initPair (Pair 1) (Color 60) (Color 233)
          initPair (Pair 2) (Color 233) (Color 60)
          screen <- initScr
          keypad screen True
          echo False
          cBreak True
          renderSquares screen [(i,j) | i <- [0..7], j <- [0..7]]
          refresh
          wait 7000000
          endWin