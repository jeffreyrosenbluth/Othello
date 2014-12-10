module Main where

import Types
import Game
import AI

import Control.Monad
import Control.Monad.Random

randIndex :: RandomGen r => Int -> Rand r Int
randIndex  len = getRandomR (0, len - 1)

randMove :: RandomGen r => Game -> Rand r Move
randMove g = do
  let ls = legalSquares g
      n  = length ls
  idx <- randIndex n
  return $ move (ls !! idx)


playGame :: RandomGen r => Int -> Rand r Game -> Rand r Game
playGame n g = do
  g'@(Game _ b) <- g
  if isOver b
  then g
  else if odd n
       then playGame (n + 1) (return $ (chooseMove g' g'))
       else do
         m <- randMove g'
         playGame (n + 1) (return $ m g')

main :: IO ()
main = do
  gs <- forM [1..100] $ \_ -> evalRandIO $ playGame 1 (return newGame)
  hs <- forM [1..100] $ \_ -> evalRandIO $ playGame 0 (return newGame)
  let resultsB = map (findWinner . board) gs
      blackWins = sum $ map (oneIfEq Black) resultsB
      resultsW = map (findWinner . board) hs
      whiteWins = sum $ map (oneIfEq White) resultsW
  print $ "Computer playing black wins "
       ++ show blackWins
       ++ " out of 100 games against random play."
  print $ "Computer playing white wins "
       ++ show whiteWins
       ++ " out of 100 games against random play."
