module Board where

import Piece

king :: Color -> Board -> [Piece]
king col board = filter (\p -> (pieceType p == King) && (color p == col)) board

occupiedPieces :: Color -> Board -> [Piece]
occupiedPieces col board = filter (\p -> color p == col) board

isChecked :: Color -> Board -> Bool
isChecked col board = any (\m -> null (king col m)) (map (\m -> executeMove m board) (allMoves (oppositeColor col) board))

isMated :: Color -> Board -> Bool
isMated col board = null (validMoves col board)

class HasMoves a where
    validMoves :: a -> Board -> [Move]
    allMoves :: a -> Board -> [Move]

instance HasMoves Color where
    validMoves col board = foldr (++) [] [validMoves piece board | piece <- occupiedPieces col board]
    allMoves col board = foldr (++) [] [allMoves piece board | piece <- occupiedPieces col board]

instance HasMoves Piece where
    validMoves piece board = filter (\m -> not (isChecked (color piece) (executeMove m board))) (allMoves piece board)
    allMoves piece board | isSliding (pieceType piece) = slideMoves piece board
                         | (pieceType piece) /= Pawn = stepMoves piece board
                         | otherwise = pawnMoves piece board

startBoard :: Board
startBoard = (menRow Black 0) ++ (pawnRow Black 1) ++ (pawnRow White 6) ++ (menRow White 7)

pawnRow :: Color -> Int -> [Piece]
pawnRow color idx = [Piece color Pawn (idx, n) | n <- [0..7]]

menRow :: Color -> Int -> [Piece]
menRow color idx = [Piece color Rook (idx, 0),
                    Piece color Knight (idx, 1),
                    Piece color Bishop (idx, 2),
                    Piece color Queen (idx, 3),
                    Piece color King (idx, 4),
                    Piece color Bishop (idx, 5),
                    Piece color Knight (idx, 6),
                    Piece color Rook (idx, 7)]