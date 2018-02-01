module Game where

import Board(startBoard,isMated,validMoves)
import Square(Board,Move,move)
import Player(Player(Player1,Player2),prompt,color,targetPiece,toPos)

type Game = [Move]

currentBoard :: Game -> Board
currentBoard [] = startBoard
currentBoard (m:ms) = (move (targetPiece m) (toPos m) lastBoard)
    where lastBoard = currentBoard ms

currentPlayer :: Game -> Player
currentPlayer game = if even (length game) then Player1 else Player2
    where even = (\n -> n == 0 || mod n 2 == 0)

play :: Game
play = play' []

play' :: Game -> Game
play' game = if isMated (color (currentPlayer game)) (currentBoard game) then game else play' (nextTurn game)

nextTurn :: Game -> Game
nextTurn game = getMove game : game

getMove :: Game -> Move
getMove game = prompt player moves
    where moves = validMoves (color player) board
          player = currentPlayer game
          board = currentBoard game