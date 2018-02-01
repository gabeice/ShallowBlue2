module Board where

import Square

king :: Color -> Board -> [Square]
king col board = filter (\p -> (pieceType p == King) && (color p == col)) board

occupiedSquares :: Color -> Board -> [Square]
occupiedSquares col board = filter (\p -> color p == col) board

isChecked :: Color -> Board -> Bool
isChecked col board = any (\m -> null (king col m)) (allMoves (oppositeColor col) board)

isMated :: Color -> Board -> Bool
isMated col board = null (validMoves col board)

class HasMoves a where
    validMoves :: a -> Board -> [Board]
    allMoves :: a -> Board -> [Board]

instance HasMoves Color where
    validMoves col board = foldr (++) [] [validMoves piece board | piece <- occupiedSquares col board]
    allMoves col board = foldr (++) [] [allMoves piece board | piece <- occupiedSquares col board]

instance HasMoves Square where
    validMoves piece board = filter (\m -> not (isChecked (color piece) board)) (allMoves piece board)
    allMoves piece board | isSliding (pieceType piece) = slideMoves piece board
                         | (pieceType piece) /= Pawn = stepMoves piece board
                         | otherwise = pawnMoves piece board

startBoard :: Board
startBoard = (menRow Black 0) ++ (pawnRow Black 1) ++ (pawnRow White 6) ++ (menRow White 7)

pawnRow :: Color -> Int -> [Square]
pawnRow color idx = [Piece color Pawn (idx, n) | n <- [0..7]]

menRow :: Color -> Int -> [Square]
menRow color idx = [Piece color Rook (idx, 0),
                    Piece color Knight (idx, 1),
                    Piece color Bishop (idx, 2),
                    Piece color Queen (idx, 3),
                    Piece color King (idx, 4),
                    Piece color Bishop (idx, 5),
                    Piece color Knight (idx, 6),
                    Piece color Rook (idx, 7)]