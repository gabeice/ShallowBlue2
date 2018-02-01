module Game where

import Player(Player(Player),PlayerType(Human),prompt,hasLost)
import Piece(Color(White,Black))
import Move(Move(Move))

type Game = [Move]

currentPlayer :: Game -> Color
currentPlayer game = if even (length game) then White else Black
    where even = (\n -> n == 0 || mod n 2 == 0)

play :: Game
play = play' []

play' :: Game -> Game
play' game = if hasLost (currentPlayer game) game then game else play' (nextTurn game)

nextTurn :: Game -> Game
nextTurn game = getMove game : game

getMove :: Game -> Move
getMove game = prompt player game
    where color = currentPlayer game
          player = (Player Human color)