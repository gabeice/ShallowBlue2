module Move where

import Piece(Piece(Piece),
             Color(White,Black),
             PieceType(Queen,Pawn),
             Dir,
             Pos,
             color,
             position,
             pieceType,
             moveDirs)

type Board = [Piece]
data Move = Move { targetPiece :: Piece, toPos :: Pos } deriving Eq

diff :: Pos -> Dir -> Pos
diff (a,b) (c,d) = (a + c, b + d)

inRange :: Int -> Bool
inRange = (\n -> n >= 0 && n <= 7)

onBoard :: Pos -> Bool
onBoard pos = inRange (fst pos) && inRange (snd pos)

offBoard :: Pos -> Bool
offBoard = not . onBoard

getPos :: Pos -> Board -> [Piece]
getPos pos board = filter ((==pos) . position) board

executeMove :: Move -> Board -> Board
executeMove (Move s p) b = move s p b

pawnTwoStep :: Move -> Bool
pawnTwoStep (Move (Piece _ t (a,_)) (b,_)) = t == Pawn && (abs (a - b)) == 2

move :: Piece -> Pos -> Board -> Board
move (Piece White Pawn pos) (0,a) board = (Piece White Queen (0,a)) : (filter (/=(Piece White Pawn pos)) board)
move (Piece Black Pawn pos) (7,a) board = (Piece Black Queen (7,a)) : (filter (/=(Piece Black Pawn pos)) board)
move (Piece c t (x1,y1)) (x2,y2) board = if t == Pawn && (null (getPos (x2,y2) board)) && x1 /= y1 && x2 /= y2
                                         then (Piece c t (x2,y2)) : (filter (\s -> (position s) /= (x1,y1) && (position s) /= (x1,y2)) board)
                                         else (Piece c t (x2,y2)) : (filter (\s -> (position s) /= (x1,y1) && (position s) /= (x2,y2)) board)

isOccupiedBy :: Pos -> Color -> Board -> Bool
isOccupiedBy pos col board = (not (null occupier)) && (color (head occupier)) == col
    where occupier = (getPos pos board)

slideMoves :: Piece -> Board -> [Move]
slideMoves piece board = [(Move piece pos) | pos <- validSlides]
    where validSlides = foldr (++) [] [(slideToCapture piece dir board) | dir <- (moveDirs (pieceType piece))]

slideToCapture :: Piece -> Dir -> Board -> [Pos]
slideToCapture piece dir board | null afterThat = allSpots
                               | isOccupiedBy (head afterThat) (color piece) board = emptySpots
                               | otherwise = (head afterThat) : emptySpots
    where allSpots = (slideOff piece dir)
          emptySpots = takeWhile notOccupied allSpots
          afterThat = dropWhile notOccupied allSpots
          notOccupied = (\p -> null (getPos p board))

slideOff :: Piece -> Dir -> [Pos]
slideOff piece dir = takeWhile onBoard (tail (iterate (\x -> diff x dir) (position piece)))

stepMoves :: Piece -> Board -> [Move]
stepMoves piece board = [(Move piece pos) | pos <- validSteps]
    where validSteps = filter (\p -> not (isOccupiedBy p (color piece) board)) possibleSteps
          possibleSteps = filter onBoard [(diff (position piece) dir) | dir <- moveDirs (pieceType piece)]

pawnMoves :: Piece -> Board -> [Move]
pawnMoves pawn board = [(Move pawn pos) | pos <- forwardMoves pawn board ++ captureMoves pawn board]

captureMoves :: Piece -> Board -> [Pos]
captureMoves (Piece Black _ pos) board = filter (\p -> isOccupiedBy p White board) cornerMoves
    where cornerMoves = filter onBoard [(diff pos (1, -1)), (diff pos (1,1))]

captureMoves (Piece White _ pos) board = filter (\p -> isOccupiedBy p Black board) cornerMoves
    where cornerMoves = filter onBoard [(diff pos (-1, 1)), (diff pos (-1,-1))]

forwardMoves :: Piece -> Board -> [Pos]
forwardMoves (Piece Black _ pos) board | (offBoard oneForward) || not (null (getPos oneForward board)) = []
                                       | (fst pos) /= 1 || not (null (getPos twoForward board)) = [oneForward]
                                       | otherwise = [oneForward, twoForward]
    where oneForward = diff pos (1,0)
          twoForward = diff pos (2,0)

forwardMoves (Piece White _ pos) board | (offBoard oneForward) || not (null (getPos oneForward board)) = []
                                       | (fst pos) /= 6 || not (null (getPos twoForward board)) = [oneForward]
                                       | otherwise = [oneForward, twoForward]
    where oneForward = diff pos (-1,0)
          twoForward = diff pos (-2,0)