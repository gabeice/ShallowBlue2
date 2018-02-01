module Move where

import Piece(Piece(Piece),Color(White,Black),Dir,Pos,color,position,pieceType,moveDirs,moveSteps)

type Board = [Piece]
data Move = Move { targetPiece :: Piece, toPos :: Pos }

diff :: Pos -> Dir -> Pos
diff pos dir = ((fst pos) + (fst dir), (snd pos) + (snd dir))

onBoard :: Pos -> Bool
onBoard pos = all (\n -> n >= 0 && n <= 7) pos

offBoard :: Pos -> Bool
offBoard pos = not (onBoard pos)

getPos :: Pos -> Board -> [Piece]
getPos pos board = filter (\p -> pos == (position p)) board

executeMove :: Move -> Board -> Board
executeMove (Move s p) b = move s p b

move :: Piece -> Pos -> Board -> Board
move piece toPos board = (Piece (color piece) (pieceType piece) toPos) : (filter (\p -> p /= piece) board)

isOccupiedBy :: Pos -> Color -> Board -> Bool
isOccupiedBy pos col board = (not (null occupier)) && (color (occupier !! 0)) == col
    where occupier = (getPos pos board)

slideMoves :: Piece -> Board -> [Move]
slideMoves piece board = [(Move piece pos) | pos <- validSlides]
    where validSlides = foldr (++) [] [(slideToCapture piece dir board) | dir <- (moveDirs (pieceType piece))]

slideToCapture :: Piece -> Dir -> Board -> [Pos]
slideToCapture piece dir board | isOccupiedBy (head afterThat) (color piece) board = emptySpots
                               | otherwise = (head afterThat) : emptySpots
    where emptySpots = takeWhile notOccupied (slideOff piece dir)
          afterThat = dropWhile notOccupied (slideOff piece dir)
          notOccupied = (\p -> null (getPos p board))

slideOff :: Piece -> Dir -> [Pos]
slideOff piece dir = takeWhile onBoard (iterate (\x -> diff x dir) (position piece))

stepMoves :: Piece -> Board -> [Move]
stepMoves piece board = [(Move piece pos) | pos <- validSteps]
    where validSteps = filter (\p -> not (isOccupiedBy p (color piece) board)) possibleSteps
          possibleSteps = filter onBoard [(diff (position piece) dir) | dir <- moveSteps (pieceType piece)]

pawnMoves :: Piece -> Board -> [Move]
pawnMoves pawn board = [(Move pawn pos) | pos <- forwardMoves pawn board ++ captureMoves pawn board]

captureMoves :: Piece -> Board -> [Pos]
captureMoves (Piece Black _ pos) board = filter (\p -> isOccupiedBy p White board) cornerMoves
    where cornerMoves = filter onBoard [(diff pos (-1, -1)), (diff pos (-1,1))]

captureMoves (Piece White _ pos) board = filter (\p -> isOccupiedBy p Black board) cornerMoves
    where cornerMoves = filter onBoard [(diff pos (1, -1)), (diff pos (1,1))]

forwardMoves :: Piece -> Board -> [Pos]
forwardMoves (Piece Black _ pos) board | (offBoard oneForward) || not (null (getPos oneForward board)) = []
                                          | (offBoard twoForward) || not (null (getPos twoForward board)) = [oneForward]
                                          | otherwise = [oneForward, twoForward]
    where oneForward = diff pos (1,0)
          twoForward = diff pos (2,0)

forwardMoves (Piece White _ pos) board | (offBoard oneForward) || not (null (getPos oneForward board)) = []
                                          | (offBoard twoForward) || not (null (getPos twoForward board)) = [oneForward]
                                          | otherwise = [oneForward, twoForward]
    where oneForward = diff pos (-1,0)
          twoForward = diff pos (-2,0)