module Game where

import Player(Player(Player),PlayerType(Human),prompt,hasLost)
import Piece(Color(White,Black))
import Move(Move(Move))
import Display(initializeDisplay,clearDisplay)

data Game = Game { log :: Log, player1 :: Player, player2 :: Player}
type Log = [Move]

currentPlayer :: Log -> Color
currentPlayer log = if even (length log) then White else Black
    where even = (\n -> n == 0 || mod n 2 == 0)

getMove :: Player -> Log -> IO Move
getMove player log = prompt player log

play' :: Game -> IO ()
play' (Game log pl1 pl2) = do if hasLost color log
                              then return ()
                              else do mv <- getMove cp log
                                      play' (Game (mv : log) pl1 pl2)
    where color = (currentPlayer log)
          cp = if color == White then pl1 else pl2

play :: IO ()
play = do screen <- initializeDisplay
          play' (Game [] (Player Human White) (Player Human Black))
          clearDisplay screen