module Square where

type Board = [Square]
data Square = Piece Color PieceType Pos deriving Eq
data Color = Black | White deriving Eq
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq
type Pos = (Int, Int)
type Dir = (Int, Int)
type Step = (Int, Int)
data Move = Move Square Pos

color :: Square -> Color
color (Piece c _ _) = c

pieceType :: Square -> PieceType
pieceType (Piece _ t _) = t

position :: Square -> Pos
position (Piece _ _ p) = p

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

isSliding :: PieceType -> Bool
isSliding King = False
isSliding Queen = True
isSliding Rook = True
isSliding Bishop = True
isSliding Knight = False
isSliding Pawn = False

moveDirs :: PieceType -> [Dir]
moveDirs Queen = [(-1,-1),(-1,0),(-1,1),(0,1),(0,-1),(1,-1),(1,0),(1,1)]
moveDirs Rook = [(-1,0),(0,-1),(0,1),(1,0)]
moveDirs Bishop = [(-1,-1),(-1,1),(1,-1),(1,1)]

moveSteps :: PieceType -> [Step]
moveSteps King = [(-1,-1),(-1,0),(-1,1),(0,1),(0,-1),(1,-1),(1,0),(1,1)]
moveSteps Knight = [(-1,-2),(-1,2),(-2,1),(-2,-1),(1,2),(2,1),(1,-2),(2,-1)]

diff :: Pos -> Dir -> Pos
diff pos dir = ((fst pos) + (fst dir), (snd pos) + (snd dir))

onBoard :: Pos -> Bool
onBoard pos = all (\n -> n >= 0 && n <= 7) pos

offBoard :: Pos -> Bool
offBoard pos = not (onBoard pos)

getPos :: Pos -> Board -> [Square]
getPos pos board = filter (\p -> pos == (position p)) board

executeMove :: Move -> Board -> Board
executeMove (Move s p) b = move s p b

move :: Square -> Pos -> Board -> Board
move piece toPos board = (Piece (color piece) (pieceType piece) toPos) : (filter (\p -> p /= piece) board)

isOccupiedBy :: Pos -> Color -> Board -> Bool
isOccupiedBy pos col board = (not (null occupier)) && (color (occupier !! 0)) == col
    where occupier = (getPos pos board)

slideMoves :: Square -> Board -> [Move]
slideMoves piece board = [(Move piece pos) | pos <- validSlides]
    where validSlides = foldr (++) [] [(slideToCapture piece dir board) | dir <- (moveDirs (pieceType piece))]

slideToCapture :: Square -> Dir -> Board -> [Pos]
slideToCapture piece dir board | isOccupiedBy (head afterThat) (color piece) board = emptySpots
                               | otherwise = (head afterThat) : emptySpots
    where emptySpots = takeWhile notOccupied (slideOff piece dir)
          afterThat = dropWhile notOccupied (slideOff piece dir)
          notOccupied = (\p -> null (getPos p board))

slideOff :: Square -> Dir -> [Pos]
slideOff piece dir = takeWhile onBoard (iterate (\x -> diff x dir) (position piece))

stepMoves :: Square -> Board -> [Move]
stepMoves piece board = [(Move piece pos) | pos <- validSteps]
    where validSteps = filter (\p -> not (isOccupiedBy p (color piece) board)) possibleSteps
          possibleSteps = filter onBoard [(diff (position piece) dir) | dir <- moveSteps (pieceType piece)]

pawnMoves :: Square -> Board -> [Move]
pawnMoves pawn board = [(Move pawn pos) | pos <- forwardMoves pawn board ++ captureMoves pawn board]

captureMoves :: Square -> Board -> [Pos]
captureMoves (Piece Black Pawn pos) board = filter (\p -> isOccupiedBy p White board) cornerMoves
    where cornerMoves = filter onBoard [(diff pos (-1, -1)), (diff pos (-1,1))]

captureMoves (Piece White Pawn pos) board = filter (\p -> isOccupiedBy p Black board) cornerMoves
    where cornerMoves = filter onBoard [(diff pos (1, -1)), (diff pos (1,1))]

forwardMoves :: Square -> Board -> [Pos]
forwardMoves (Piece Black Pawn pos) board | (offBoard oneForward) || not (null (getPos oneForward board)) = []
                                          | (offBoard twoForward) || not (null (getPos twoForward board)) = [oneForward]
                                          | otherwise = [oneForward, twoForward]
    where oneForward = diff pos (1,0)
          twoForward = diff pos (2,0)

forwardMoves (Piece White Pawn pos) board | (offBoard oneForward) || not (null (getPos oneForward board)) = []
                                          | (offBoard twoForward) || not (null (getPos twoForward board)) = [oneForward]
                                          | otherwise = [oneForward, twoForward]
    where oneForward = diff pos (-1,0)
          twoForward = diff pos (-2,0)