module BitBoard where

import Data.Bits
import Data.Word
import Numeric
import Data.Char
import Data.List
import Data.List.Split

type Point = (Int, Int)

-- a1 = (0,0) = 1 :: Word64, h8 = (7, 7) = 2^63.
type Board = Word64

(<<<) :: Board -> Int -> Board
(<<<) = unsafeShiftL

(>>>) :: Board -> Int -> Board
(>>>) = unsafeShiftR

(.^.) :: Board -> Board -> Board
(.^.) = xor

initialBoardBlack :: Board
initialBoardBlack = 0x1008000000

initialBoardWhite :: Board
initialBoardWhite = 0x810000000


rightMask :: Board
rightMask = 0x8080808080808080

rightMaskNot :: Board
rightMaskNot = 0x7F7F7F7F7F7F7F7F

leftMask :: Board
leftMask = 0x101010101010101 

leftMaskNot :: Board
leftMaskNot = 0xFEFEFEFEFEFEFEFE

diagA1H8 :: Board
diagA1H8 = 0x8040201008040201

diagH1A8 :: Board
diagH1A8 = 0x102040810204080

fromPoint :: Point -> Board
fromPoint (i, j) = (1 <<< i) <<< (8 * j)

toPoint :: Board -> Point
toPoint b = (j, i)
  where
    (i, j) = quotRem (floor x) 8
    x = logBase 2 (fromIntegral b) :: Double

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

legalMovesDir :: Board -> Board -> (Board -> Board) -> Board
legalMovesDir player opponent dfun = foldr1 (.|.) ms
  where
    empty = complement (player .|. opponent)
    os = scanl1 (.&.) . drop 1 $ iterate dfun opponent
    ps = drop 2 $ iterate dfun player
    es = replicate 6 empty
    combine x y z = x .&. y .&. z
    ms = zipWith3 combine os es ps

legalMoves :: Board -> Board -> Board
legalMoves p o = foldr1 (.|.) ms
  where
    dfuns = [shiftN, shiftNE, shiftE, shiftSE, shiftS, shiftSW, shiftW, shiftNW]
    ms    = map (legalMovesDir p o) dfuns

-- col0row0 . row0col0 == A1..A8 = row0col0 . col0row0
-- A1..A8 to H1..A1
col0row0 :: Board -> Board
col0row0 x = ((x .&. leftMask) * diagA1H8) >>> 56 

-- A1..H1 to A8..A1
row0col0 :: Board -> Board
row0col0 x = ((x .&. 0xFF) * diagA1H8) >>> 7 .&. leftMask

a1h8row0 :: Board -> Board
a1h8row0 x = ((x .&. diagA1H8) * leftMask) >>> 56

h1a8row0 :: Board -> Board
h1a8row0 x = ((x .&. diagH1A8) * leftMask) >>> 56

-- Perhaps try c_bswap64
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
