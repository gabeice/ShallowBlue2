module Display where

import UI.HSCurses.Curses
import Piece(Board,Move)

data Display = Display { board :: Board }

render :: Display -> IO ()

printMessage :: Display -> String -> IO ()

getMove :: Display -> [Move] -> IO (Move)

darkBrown :: IO ()
darkBrown = initColor (Color 3) (139, 69, 19)

lightBrown :: IO ()
lightBrown = initColor (Color 4) (245, 222, 179)