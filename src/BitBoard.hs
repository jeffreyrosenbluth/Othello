-----------------------------------------------------------------------------
-- |
-- Module      :  BitBoard
-- Copyright   :  (c) 2014 Jeffrey Rosenbluth
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- References  : https://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating#FlipabouttheDiagonal
--               http://rit-othello.googlecode.com/svn/trunk/Othello/src/core/OthelloBitBoard.java

-- Bitboard manipulations from Othello.
--
-----------------------------------------------------------------------------

module BitBoard where

import Types hiding (Board)

import Data.Array.Unboxed
import Data.Bits
import Data.Word
import Numeric
import Data.Char
import Data.List
import Data.List.Split

type Point = (Int, Int)

-- | a1 = (0,0) = 1 :: Word64, h8 = (7, 7) = 63.
type Board = Word64

-- | Useful operators, boards and masks.
(<<<) :: Board -> Int -> Board
(<<<) = unsafeShiftL

(>>>) :: Board -> Int -> Board
(>>>) = unsafeShiftR

(.^.) :: Board -> Board -> Board
(.^.) = xor

initialBoardBlack, initialBoardWhite, rightMask, rightMaskNot, leftMask, leftMaskNot
   , diagA1H8, diagH1A8
  :: Board
initialBoardBlack = 0x1008000000
initialBoardWhite = 0x810000000
rightMask         = 0x8080808080808080
rightMaskNot      = 0x7F7F7F7F7F7F7F7F
leftMask          = 0x101010101010101 
leftMaskNot       = 0xFEFEFEFEFEFEFEFE
diagA1H8          = 0x8040201008040201
diagH1A8          = 0x102040810204080

asterisk :: UArray Point Board
asterisk =  ixmap ((1,1), (8,8)) (\(x,y) -> (y,x)) $ listArray ((1,1), (8,8))
  [
    0x81412111090503FE, 0x02824222120A07FD, 0x0404844424150EFB, 0x08080888492A1CF7, 
    0x10101011925438EF, 0x2020212224A870DF, 0x404142444850E0BF, 0x8182848890A0C07F, 
    0x412111090503FE03, 0x824222120A07FD07, 0x04844424150EFB0E, 0x080888492A1CF71C, 
    0x101011925438EF38, 0x20212224A870DF70, 0x4142444850E0BFE0, 0x82848890A0C07FC0, 
    0x2111090503FE0305, 0x4222120A07FD070A, 0x844424150EFB0E15, 0x0888492A1CF71C2A, 
    0x1011925438EF3854, 0x212224A870DF70A8, 0x42444850E0BFE050, 0x848890A0C07FC0A0, 
    0x11090503FE030509, 0x22120A07FD070A12, 0x4424150EFB0E1524, 0x88492A1CF71C2A49, 
    0x11925438EF385492, 0x2224A870DF70A824, 0x444850E0BFE05048, 0x8890A0C07FC0A090, 
    0x090503FE03050911, 0x120A07FD070A1222, 0x24150EFB0E152444, 0x492A1CF71C2A4988, 
    0x925438EF38549211, 0x24A870DF70A82422, 0x4850E0BFE0504844, 0x90A0C07FC0A09088, 
    0x0503FE0305091121, 0x0A07FD070A122242, 0x150EFB0E15244484, 0x2A1CF71C2A498808, 
    0x5438EF3854921110, 0xA870DF70A8242221, 0x50E0BFE050484442, 0xA0C07FC0A0908884, 
    0x03FE030509112141, 0x07FD070A12224282, 0x0EFB0E1524448404, 0x1CF71C2A49880808, 
    0x38EF385492111010, 0x70DF70A824222120, 0xE0BFE05048444241, 0xC07FC0A090888482, 
    0xFE03050911214181, 0xFD070A1222428202, 0xFB0E152444840404, 0xF71C2A4988080808, 
    0xEF38549211101010, 0xDF70A82422212020, 0xBFE0504844424140, 0x7FC0A09088848281
  ]                                                     

-- | Convert a 'Point' to a 'Board' with a single bit set.
fromPoint :: Point -> Board
fromPoint (i, j) = (1 <<< i) <<< (8 * j)

-- | Convert a 'Board' with a single bit to a 'Point'
--   The 'Board' must have only a single bit set.
toPoint :: Board -> Point
toPoint b
  | popCount b == 1 = (j, i)
  | otherwise = error "Only boards with a single bit set can be converted to points."
  where
    (i, j) = quotRem (floor x) 8
    x = logBase 2 (fromIntegral b) :: Double

applyN :: (a -> a) -> Int -> a -> a
applyN f n x = iterate f x !! n

showBoard :: Board -> String
showBoard brd = '\n' : sqrs ++ "\n    - - - - - - - -"
                            ++ "\n    a b c d e f g h\n\n"
  where
    bin  = showIntAtBase 2 intToDigit brd ""
    n    = 64 - length bin
    pad  = replicate n '0'
    str  = intersperse ' ' (reverse $ pad ++ bin)
    rows = reverse $ chunksOf 16 str
    xs   = zipWith (\a b -> show a ++ " | " ++ b) ([8,7..1] :: [Int]) rows
    sqrs = intercalate "\n" xs

-- | Board shifts in all 8 directions, bits shfited past the edges of
--   the board fall off.
shiftN :: Board -> Board
shiftN b = b <<< 8

shiftS :: Board -> Board
shiftS b = b >>> 8

shiftE :: Board -> Board
shiftE b = b <<< 1 .&. leftMaskNot

shiftW :: Board -> Board
shiftW b = b >>> 1 .&. rightMaskNot

shiftNE :: Board -> Board
shiftNE b = b <<< 8 <<< 1 .&. leftMaskNot

shiftSE :: Board -> Board
shiftSE b = b >>> 8 <<< 1 .&. leftMaskNot

shiftSW :: Board -> Board
shiftSW b = b >>> 8 >>> 1 .&. rightMaskNot

shiftNW :: Board -> Board
shiftNW b = b <<< 8 >>> 1 .&. rightMaskNot

-- | A 'Board' representing the legal moves in one direction.
--   For 'player' with 'opponent'.
--   'dfun' extracts the the direction, e.g. 'defun = shiftE'.
legalMovesDir :: Board -> Board -> (Board -> Board) -> Board
legalMovesDir player opponent dfun = foldr1 (.|.) ms
  where
    empty = complement (player .|. opponent)
    os = scanl1 (.&.) . drop 1 $ iterate dfun opponent
    ps = drop 2 $ iterate dfun player
    es = replicate 6 empty
    combine x y z = x .&. y .&. z
    ms = zipWith3 combine os es ps

-- | All the leagal moves for player p, given opponent o
legalMoves :: Board -> Board -> Board
legalMoves p o = foldr1 (.|.) ms
  where
    dfuns = [shiftN, shiftNE, shiftE, shiftSE, shiftS, shiftSW, shiftW, shiftNW]
    ms    = map (legalMovesDir p o) dfuns

-- | Transformations to bring the principal diagnals and the first column
--   to the first row and back.
--   'col0row0 . row0col0 == A1..A8 = row0col0 . col0row0'
--   A1..A8 to H1..A1
col0row0 :: Board -> Board
col0row0 x = ((x .&. leftMask) * diagA1H8) >>> 56 

-- | A1..H1 to A8..A1
row0col0 :: Board -> Board
row0col0 x = ((x .&. 0xFF) * diagA1H8) >>> 7 .&. leftMask

a1h8row0 :: Board -> Board
a1h8row0 x = ((x .&. diagA1H8) * leftMask) >>> 56

row0a1h8 :: Board -> Board
row0a1h8 x = x4
  where
    x0 = x .&. 0xFF
    x1 = x0 .|. (x0 <<< 8)
    x2 = x1 .|. (x1 <<< 16)
    x3 = x2 .|. (x2 <<< 32)
    x4 = x3 .&. 0x8040201008040201

h1a8row0 :: Board -> Board
h1a8row0 x = ((x .&. diagH1A8) * leftMask) >>> 56

row0h1a8 :: Board -> Board
row0h1a8 x = x4
  where
    x0 = x .&. 0xFF
    x1 = x0 .|.  (x0 <<< 8)
    x2 = x1 .|. ((x1 .&. 0x1122) <<< 16)
    x3 = x2 .|.  (x2 <<< 32)
    x4 = x3 .&. 0x0102040810204080

row0List :: Board -> [Bool]
row0List x = map (testBit x) [0..7]

-- | Functions to transform an entire 'Board'
-- XXX Perhaps try c_bswap64
flipVertical :: Board -> Board
flipVertical x = x3
  where
    x1 = ((x  >>>  8) .&. k1) .|. ((x  .&. k1) <<< 8)
    x2 = ((x1 >>> 16) .&. k2) .|. ((x1 .&. k2) <<< 16)
    x3 = ( x2 >>> 32)         .|. ( x2         <<< 32)
    k1 = 0x00FF00FF00FF00FF
    k2 = 0x0000FFFF0000FFFF
 
mirrorHorizontal :: Board -> Board
mirrorHorizontal x = x3
  where
    x1 = ((x  >>> 1) .&. k1) +  2 * (x  .&. k1)
    x2 = ((x1 >>> 2) .&. k2) +  4 * (x1 .&. k2)
    x3 = ((x2 >>> 4) .&. k4) + 16 * (x2 .&. k4)
    k1 = 0x5555555555555555
    k2 = 0x3333333333333333
    k4 = 0x0f0f0f0f0f0f0f0f

flipDiagA1H8 :: Board -> Board
flipDiagA1H8 x = x3
  where
    t1 = k4 .&. (x  .^. (x  <<< 28))
    x1 = x  .^. (t1 .^. (t1 >>> 28))
    t2 = k2 .&. (x1 .^. (x1 <<< 14))
    x2 = x1 .^. (t2 .^. (t2 >>> 14))
    t3 = k1 .&. (x2 .^. (x2 <<<  7))
    x3 = x2 .^. (t3 .^. (t3 >>>  7))
    k1 = 0x5500550055005500
    k2 = 0x3333000033330000
    k4 = 0x0f0f0f0f00000000

flipDiagA8H1 :: Board -> Board
flipDiagA8H1 x = x3
  where
   t1 = x  .^.                 (x  <<< 36) 
   x1 = x  .^. (k4 .&. (t1 .^. (x  >>> 36)))
   t2 = k2 .&. (x1 .^.         (x1 <<< 18))
   x2 = x1 .^.         (t2 .^. (t2 >>> 18))
   t3 = k1 .&.         (x2 .^. (x2 <<<  9))
   x3 = x2 .^.         (t3 .^. (t3 >>>  9))
   k1 = 0xaa00aa00aa00aa00
   k2 = 0xcccc0000cccc0000
   k4 = 0xf0f0f0f00f0f0f0f

rotate90 :: Board -> Board
rotate90 = flipDiagA1H8 . flipVertical

rotate180 :: Board -> Board
rotate180 = mirrorHorizontal . flipVertical

rotate270 :: Board -> Board
rotate270 = flipVertical . flipDiagA1H8

opposite :: Piece -> Piece
opposite Black = White
opposite White = Black
opposite Empty = Empty

flipList :: Piece -> [Piece] -> [Piece]
flipList p xs
  | null qs || null ps = xs
  | head ps == p = map opposite qs ++ ps 
  | otherwise = xs
  where
    (qs, ps) = span (== opposite p) xs
            
boardsToList :: Board -> Board -> [Piece]
boardsToList black white = zipWith c b w
  where
    b = row0List black
    w = row0List white
    c x y
      | x = Black
      | y = White
      | otherwise = Empty

bin2dec :: String -> Board
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

listToBoards :: [Piece] -> (Board, Board)
listToBoards ps = (bin2dec black, bin2dec white)
  where
    black = concatMap (\p -> case p of {Black -> "1"; _ -> "0"}) ps'
    white = concatMap (\p -> case p of {White -> "1"; _ -> "0"}) ps'
    ps' = reverse ps

lineToLists :: Int -> Board -> Board -> ([Piece], [Piece])
lineToLists m b w = (reverse l, drop 1 r)
  where
    ps = boardsToList b w
    (l, r) = splitAt m ps

rowToLists :: Point -> Board -> Board -> ([Piece], [Piece])
rowToLists (i, j) b w = lineToLists i b0 w0
  where
    (b0, w0) = (applyN shiftS j b, applyN shiftS j w)

colToLists :: Point -> Board -> Board -> ([Piece], [Piece])
colToLists (i, j) b w = lineToLists j b0 w0
  where
    (b0', w0') = (applyN shiftW i b, applyN shiftW i w)
    (b0, w0)   = (col0row0 b0', col0row0 w0')

diagPToLists :: Point -> Board -> Board -> ([Piece], [Piece])
diagPToLists (i, j) b w = lineToLists j b0 w0
  where
    (b0', w0') = if i > j
                 then (applyN shiftW (i - j) b, applyN shiftW (i - j) w)
                 else (applyN shiftE (j - i) b, applyN shiftE (j - i) w)
    (b0, w0)   = (a1h8row0 b0', a1h8row0 w0')
          
diagNToLists :: Point -> Board -> Board -> ([Piece], [Piece])
diagNToLists (i, j) b w = lineToLists j b0 w0
  where
    (b0', w0') = if i < j
                 then (applyN shiftE (7 - j + i) b, applyN shiftE (7 - j + i) w)
                 else (applyN shiftW (7 - i + j) b, applyN shiftW (7 - i + j) w)
    (b0, w0)   = (h1a8row0 b0', h1a8row0 w0')

testBoard :: Board
testBoard
  =   fromPoint (1,0) .|. fromPoint (5,0)
  .|. fromPoint (1,1) .|. fromPoint (4,1)
  .|. fromPoint (1,2) .|. fromPoint (3,2)
  .|. fromPoint (1,3) .|. fromPoint (2,3) .|. fromPoint (3,3)
  .|. fromPoint (1,4) .|. fromPoint (4,4)
  .|. fromPoint (1,5) .|. fromPoint (5,5)
  .|. fromPoint (1,6) .|. fromPoint (5,6)
  .|. fromPoint (1,7) .|. fromPoint (2,7) .|. fromPoint (3,7) .|. fromPoint (4,7)

