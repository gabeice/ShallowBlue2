module Game where

import Board(startBoard,isMated,validMoves)
import Square(Board,Color(White,Black),Move,move,targetPiece,toPos)
import Player(Player(Player),PlayerType(Human),prompt,color)

type Game = [Move]

currentBoard :: Game -> Board
currentBoard [] = startBoard
currentBoard (m:ms) = (move (targetPiece m) (toPos m) lastBoard)
    where lastBoard = currentBoard ms

currentPlayer :: Game -> Color
currentPlayer game = if even (length game) then White else Black
    where even = (\n -> n == 0 || mod n 2 == 0)

play :: Game
play = play' []

play' :: Game -> Game
play' game = if isMated (currentPlayer game) (currentBoard game) then game else play' (nextTurn game)

nextTurn :: Game -> Game
nextTurn game = getMove game : game

getMove :: Game -> Move
getMove game = prompt player board moves
    where moves = validMoves color board
          board = currentBoard game
          color = currentPlayer game
          player = (Player Human color)