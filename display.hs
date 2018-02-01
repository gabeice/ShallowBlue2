module Display where

import UI.HSCurses.Curses
import Square(Board)

data Display = Display Board

render :: Display -> IO ()

darkBrown :: IO ()
darkBrown = initColor (Color 3) (139, 69, 19)

lightBrown :: IO ()
lightBrown = initColor (Color 4) (245, 222, 179)