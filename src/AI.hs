--------------------------------------------------------------------------------------
-- AI
--------------------------------------------------------------------------------------
module AI where

import Types
import Game

import Data.Array
import Data.Function (on)
import Data.List (maximumBy)
--------------------------------------------------------------------------------------

legalSquares :: Game -> [Square]
legalSquares (Game p b) = filter (isLegal b p) squares

legalMoves :: Game -> [(Game, Square)]
legalMoves g@(Game p b) = zip gs ls
  where
    ls = filter (isLegal b p) squares
    gs = map (flip move g) ls

chooseMove :: Game -> Move
chooseMove g = move (snd best)
  where
    candidates = legalMoves g
    scored = map (\(h, s) -> (heuristic h, s)) candidates
    best = maximumBy (compare `on` fst) scored

nextMove :: Game -> String
nextMove g = show $ (\(x, y) -> (x, 9 - y)) (snd best)
  where
    candidates = legalMoves g
    scored = map (\(h, s) -> (heuristic h, s)) candidates
    best = maximumBy (compare `on` fst) scored

--------------------------------------------------------------------------------------
-- Heuristic, based on:
-- http://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello
--------------------------------------------------------------------------------------

-- Assing a score to a board based on the subsequent criteria.
-- See:
-- http://courses.cs.washington.edu/courses/cse573/04au/Project/mini1/RUSSIA/Final_Paper.pdf
heuristic :: Game -> Double
heuristic g =  10.0   * parity g
            + 801.724 * cornerOcc g
            + 382.026 * cornerAdj g
            +  78.922 * mobility g
            +  74.396 * stability g
            +  10.0   * squareValues g  
  
oneIfEq :: Eq a => a -> a -> Double
oneIfEq p q = if p == q then 1 else 0
                
-- Value for occupying more squares.
parity :: Game -> Double
parity (Game p b)
  | me  > you =  100 * me  / total
  | you > me  = -100 * you / total
  | otherwise = 0
  where
    ps = elems b
    mes = map (oneIfEq (opposite p)) ps
    yous = map (oneIfEq p) ps
    (me, you) = (sum mes, sum yous)
    total = me + you

-- Values for occupying specific squares.
valueTable :: Array Square Double
valueTable = listArray ((1,1), (8,8)) valList
  where
    valList = [ 20, -3, 11,  8,  8, 11, -3, 20
              , -3, -7, -4,  1,  1, -4, -7, -3
              , 11, -4,  2,  2,  2,  2, -4, 11
              ,  8,  1,  2, -3, -3,  2,  1,  8
              ,  8,  1,  2, -3, -3,  2,  1,  8
              , 11, -4,  2,  2,  2,  2, -4, 11
              , -3, -7, -4,  1,  1, -4, -7, -3
              , 20, -3, 11,  8,  8, 11, -3, 20 ]

squareValue :: Board -> Piece -> Square -> Double
squareValue b p s
  | q == p = valueTable ! s
  | q == opposite p = - (valueTable ! s)
  | otherwise = 0
  where
    q = b ! s
              
squareValues :: Game -> Double
squareValues (Game p b) = sum $ map (squareValue b (opposite p)) squares

-- Index offsets to 8 adjacent squares.
frontierX :: Array Int Int
frontierX = listArray (1, 8) [-1, -1, 0, 1, 1, 1, 0, -1]

frontierY :: Array Int Int
frontierY = listArray (1, 8) [0, 1, 1, 1, 0, -1, -1, -1]

-- Measures the potential for a square to be flanked.
stable :: Board -> Piece -> Square -> Double
stable b p s
  | b ! s == Empty = 0
  | otherwise = sum $ map (oneIfEq p . (b !)) goodSqs
  where
    (i, j) = s
    sqs = [(i + frontierX ! k, j + frontierY ! k) | k <- [1..8]] 
    goodSqs = filter (\(x, y) -> x >= 1 && x <= 8 && y >= 1 && y <= 8) sqs

stability :: Game -> Double
stability (Game p b)
  | me  > you = -100 * me  / total
  | you > me  =  100 * you / total
  | otherwise = 0
  where
    mes    = map (stable b (opposite p)) squares
    yous   = map (stable b p)            squares
    (me, you) = (sum mes, sum yous)
    total = me + you
    
unitVal :: Piece -> Piece -> Double
unitVal p q
  | q == p = 1
  | q == opposite p = (-1)
  | otherwise = 0

-- Occupying a corner is very good.
cornerOcc :: Game -> Double
cornerOcc (Game p b) = (25 *) . sum $ map (unitVal (opposite p)) corners
  where
    corners = [b ! (1,1), b ! (1,8), b ! (8,1), b ! (8,8)]

-- Occupying a square adjacent to a corner is bad.
cornerAdj :: Game -> Double
cornerAdj (Game p b) = ((-12.5) *) . sum
                     $ map (unitVal (opposite p)) (concat [ll, lr, tr, tl])
  where
    ll = if b ! (1,1) == Empty
         then [b ! (1,2), b ! (2,2), b ! (2,1)]
         else []
    lr = if b ! (8,1) == Empty
         then [b ! (7,1), b ! (7,2), b ! (8,2)]
         else []
    tr = if b ! (8,8) == Empty
         then [b ! (8,7), b ! (7,7), b ! (7,8)]
         else []
    tl = if b ! (1,8) == Empty
         then [b ! (1,7), b ! (2,7), b ! (2,8)]
         else []

-- Measures how many move choice you have relative to your opponent.
mobility :: Game -> Double
mobility g
  | me > you = 100 * me / total
  | you > me = -100 * you / total
  | otherwise = 0
  where
    you = fromIntegral . length $ (legalSquares g)
    me = fromIntegral . length $ (legalSquares h)
    h = g { piece = opposite (piece g) }
    total = me + you
