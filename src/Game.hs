module Game where

import Types

import Data.Array

----------------------------------------------------------------------
-- Game Logic
----------------------------------------------------------------------
line :: Square -> Direction -> Line
line (x, y) N  = [(x, y + h)     | h <- [1..8], y+h <= 8]
line (x, y) S  = [(x, y - h)     | h <- [1..8], y-h >= 1]
line (x, y) E  = [(x + h, y)     | h <- [1..8], x+h <= 8]
line (x, y) W  = [(x - h, y)     | h <- [1..8], x-h >= 1]
line (x, y) NE = [(x + h, y + h) | h <- [1..8], y+h <= 8, x+h <= 8]
line (x, y) SE = [(x + h, y - h) | h <- [1..8], y-h >= 1, x+h <= 8]
line (x, y) NW = [(x - h, y + h) | h <- [1..8], y+h <= 8, x-h >= 1]
line (x, y) SW = [(x - h, y - h) | h <- [1..8], y-h >= 1, x-h >= 1]

pieces :: Board -> Line -> [Piece]
pieces brd = map (brd !)

opposite :: Piece -> Piece
opposite Black = White
opposite White = Black
opposite Empty = Empty

newBoard :: Board
newBoard = emptyArray // [((4,4), White),((4,5), Black),((5,4), Black),((5,5), White)]
  where
    emptyArray = listArray ((1,1),(8,8)) (repeat Empty)

newGame :: Game
newGame = Game Black newBoard

setSquare :: Board -> Square -> Piece -> Board
setSquare brd square p =
    if (brd ! square) /= Empty
    then error $ "square " ++ show square ++ " is not empty"
    else brd // [(square, p)]

toFlip :: Board -> Piece -> Line -> Line
toFlip _ _ []   = []
toFlip _ _ (_:[]) = []
toFlip b p l
  | zs /= [] && fst (head zs) == p = map snd ys 
  | otherwise = []
  where
    a  = zip (pieces b l) l
    ys = takeWhile (\y -> fst y == opposite p) a
    zs = dropWhile (\y -> fst y == opposite p) a

toFlipAll :: Board -> Piece -> Square -> [Square]
toFlipAll b p s = concat [toFlip b p l | l <- map (line s) [N .. NW]]

flipBoard :: Board -> Piece -> Square -> Board
flipBoard b p s = b // ((s, p) : zip flips (repeat p))
  where
    flips = toFlipAll b p s

isLegal :: Board -> Piece -> Square -> Bool
isLegal b p s = b ! s == Empty && (not . null $ toFlipAll b p s)

move :: Square -> Game -> Game
move square g@(Game p b)
  | isLegal b p square = Game piece' board'
  | otherwise          = g
  where
    board'  = flipBoard b p square
    q = opposite p
    piece'
      | any (isLegal board' q ) squares = q
      | otherwise = p

isOver :: Board -> Bool
isOver b = not (any (isLegal b Black) squares || any (isLegal b White) squares)

findWinner :: Board -> Piece
findWinner b
  | isOver b = case compare black white of
      GT -> Black
      LT -> White
      EQ -> Empty -- We use Empty to indicate a draw.
  | otherwise = error "The game is not over"
  where
    black = length $ filter (\s -> b ! s == Black) squares
    white = length $ filter (\s -> b ! s == White) squares
