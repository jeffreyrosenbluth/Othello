----------------------------------------------------------------------
-- Types for othello
----------------------------------------------------------------------

module Types where

import           Data.Array

data Piece = Empty | Black | White
  deriving (Show, Eq)

type Square = (Int, Int)

type Line = [Square]

type Board = Array Square Piece

type Move = Game -> Game

data Game = Game { piece :: Piece, board :: Board }

squares :: [Square]
squares = [(x, y) | y <- [1..8], x <- [1..8]]
