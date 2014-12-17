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

initialBoardBlack :: Board
initialBoardBlack = 68853694464

initialBoardWhite :: Board
initialBoardWhite =  34628173824

rightMask :: Board
rightMask = complement 9259542123273814144

leftMask :: Board
leftMask = complement 72340172838076673

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
shiftE b = b <<< 1 .&. leftMask

shiftW :: Board -> Board
shiftW b = b >>> 1 .&. rightMask

shiftNE :: Board -> Board
shiftNE b = b <<< 8 <<< 1 .&. leftMask

shiftSE :: Board -> Board
shiftSE b = b >>> 8 <<< 1 .&. leftMask

shiftSW :: Board -> Board
shiftSW b = b >>> 8 >>> 1 .&. rightMask

shiftNW :: Board -> Board
shiftNW b = b <<< 8 >>> 1 .&. rightMask

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

