--------------------------------------------------------------------------------------
-- Profiling
--------------------------------------------------------------------------------------
module Main where

import Types
import Game
import AI


playGame ::Game -> Game
playGame g@(Game _ b)
  | isOver b = g
  | otherwise =  playGame (nextMove 4 g g)

main :: IO ()
main = do
  let g = playGame newGame
  print $ findWinner . board $ g
