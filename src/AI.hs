--------------------------------------------------------------------------------------
-- AI
--------------------------------------------------------------------------------------
module AI where

import           Game
import           Types

import           Data.Array
import           Data.Function (on)
import           Data.List     (foldl', maximumBy, minimumBy)
import           Data.Tree
--------------------------------------------------------------------------------------

-- Order the moves tried to maximize the benefit of alpha-beta pruning.
abSquares :: [Square]
abSquares = reverse
            [ (1,1), (1,8), (8,1), (8,8)                             --  20
            , (3,1), (6,1), (1,3), (8,3), (1,6), (8,6), (3,8), (6,8) --  11
            , (4,1), (5,1), (1,4), (8,4), (1,5), (8,5), (4,8), (5,8) --   8
            , (3,3), (4,3), (5,3), (6,3), (3,4), (6,4)               --   2
            , (3,5), (6,5), (3,6), (4,6), (5,6), (6,6)               --   2
            , (4,2), (5,2), (2,4), (7,4), (2,5), (7,5), (4,7), (5,7) --   1
            , (2,1), (7,1), (1,2), (8,2), (1,7), (8,7), (2,8), (7,8) --  -3
            , (3,2), (6,2), (2,3), (7,3), (2,6), (7,6), (3,7), (6,7) --  -4
            , (2,2), (7,2), (2,7), (7,7)                             --  -7
            ]

children :: Game -> [Game]
children g@(Game p b) = map (\s -> move p s g) (filter (isLegal b p) abSquares)

--------------------------------------------------------------------------------------
-- Minimax
--------------------------------------------------------------------------------------
type GameTree = Tree Game

alphaBeta :: GameTree -> Double
alphaBeta = go (-1/0) (1/0)
  where
    go :: Double -> Double -> GameTree -> Double
    go _ _ (Node g []) = heuristic (board g) (piece g)
    go a b (Node _ gs) = fst $ foldl' prune (a, b) gs
      where
        prune (a', b') n
          | b' < a' = (a', b')
          | otherwise = (max a (- go (-b') (-a') n), b')

gameTree :: Game -> GameTree
gameTree g = Node g (map gameTree (children g))

cutoff :: Int -> GameTree -> GameTree
cutoff 0 (Node g _) = Node g []
cutoff n (Node g gs) = Node g (map (cutoff (n - 1)) gs)

nextMove :: Int -> Piece -> Game -> Move
nextMove n p g = move p (snd best)
  where
    gt = cutoff n . gameTree
    ms = legalMoves g
    scores = map (\(g', s) -> (alphaBeta . gt $ g', s)) ms
    best = minimumBy (compare `on` fst) scores

-- For comparison.
minimax :: Piece -> GameTree -> Double
minimax q (Node g []) = heuristic (board g) q
minimax q (Node (Game p _) xs)
  | p == q = maximum (map (minimax q) xs)
  | otherwise = minimum (map (minimax q) xs)

mmNextMove :: Int -> Piece -> Game -> Move
mmNextMove n player g@(Game p _) = move player (snd best)
  where
    gt = cutoff n . gameTree
    ms = legalMoves g
    scores = map (\(g', s) -> (minimax p . gt $ g', s)) ms
    best = maximumBy (compare `on` fst) scores

-------------------------------------------------------------------------------------
-- Heuristic, based on:
-- http://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello
--------------------------------------------------------------------------------------

-- Assign a score to a board based on the subsequent criteria.
-- See:
-- http://courses.cs.washington.edu/courses/cse573/04au/Project/mini1/RUSSIA/Final_Paper.pdf
heuristic :: Board -> Piece -> Double
heuristic b p =  10.0   * parity b p
              + 801.724 * cornerOcc b p
              + 382.026 * cornerAdj b p
              +  78.922 * mobility b p
              -- +  74.396 * stability b p
              +  10.0   * squareValues b p

oneIfEq :: Eq a => a -> a -> Double
oneIfEq p q = if p == q then 1 else 0

-- Value for occupying more squares.
parity :: Board -> Piece -> Double
parity b p
  | sup  > inf =  100 * sup / total
  | inf > sup  = -100 * inf / total
  | otherwise = 0
  where
    ps = elems b
    sups = map (oneIfEq p) ps
    infs = map (oneIfEq (opposite p)) ps
    (sup, inf) = (sum sups, sum infs)
    total = sup + inf

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

squareValues :: Board -> Piece -> Double
squareValues b p = sum $ map (squareValue b p) abSquares

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

stability :: Board -> Piece -> Double
stability b p
  | sup  > inf = -100 * sup  / total
  | inf > sup  =  100 * inf / total
  | otherwise = 0
  where
    sups       = map (stable b p) abSquares
    infs       = map (stable b (opposite p)) abSquares
    (sup, inf) = (sum sups, sum infs)
    total      = sup + sup

unitVal :: Piece -> Piece -> Double
unitVal p q
  | q == p = 1
  | q == opposite p = -1
  | otherwise = 0

-- Occupying a corner is very good.
cornerOcc :: Board -> Piece -> Double
cornerOcc b p = (25 *) . sum $ map (unitVal p) corners
  where
    corners = [b ! (1,1), b ! (1,8), b ! (8,1), b ! (8,8)]

-- Occupying a square adjacent to a corner is bad.
cornerAdj :: Board -> Piece -> Double
cornerAdj b p = ((-12.5) *) . sum
                     $ map (unitVal p) (concat [ll, lr, tr, tl])
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
mobility :: Board -> Piece -> Double
mobility b p
  | sup > inf = 100 * sup / total
  | inf > sup = -100 * inf / total
  | otherwise = 0
  where
    sup  = fromIntegral . length $ legalSquares g
    inf = fromIntegral . length $ legalSquares h
    g = Game p b
    h = g { piece = opposite (piece g) }
    total = sup + inf
