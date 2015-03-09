{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import           Types
import           Game
import           AI

import           Control.Monad
import           Data.Array
import           Data.List.Split
import           System.Directory

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (on)

----------------------------------------------------------------------

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  let static = currentDirectory ++ "/static"
  startGUI defaultConfig { tpStatic = Just static } setup

getPieceUrl :: Piece -> FilePath
getPieceUrl Empty = "static/images/tile.png"
getPieceUrl Black = "static/images/black.png"
getPieceUrl White = "static/images/white.png"

initImgs :: [FilePath]
initImgs = replicate 27 (getPieceUrl Empty)
        ++ [getPieceUrl White] ++ [getPieceUrl Black]
        ++ replicate 6  (getPieceUrl Empty)
        ++ [getPieceUrl Black] ++ [getPieceUrl White]
        ++ replicate 27 (getPieceUrl Empty)

toUrls :: Game -> [FilePath]
toUrls (Game _ b) = [getPieceUrl $ b ! s | s <- squares]

showOpacity :: Bool -> [(String, String)]
showOpacity b = if b then [("opacity", "0.6")] else [("opacity", "1")]

showNotification :: Game -> String
showNotification (Game p b)
  | isOver b = (show $ findWinner b) ++ " player wins!"
  | otherwise = case p of
      White -> "White thinking"
      Black -> "Blacks's turn"
      Empty -> ""

  -- | otherwise = show p ++ "'s turn"

union :: Event a -> Event a -> Event a
union = unionWith const

buildGameState :: [Element] -> UI (Behavior Game)
buildGameState imgs = do
  eState <- accumE newGame moves
  stepper newGame eState
    where
      ePlayer   = fmap concatenate . unions $ zipWith (\e s -> move Black s <$ e)
                  (map UI.mousedown imgs) squares
      eMachine  = (\s -> nextMove 3 White  s s) <$ unions (UI.mouseup  <$> imgs)
      moves     = union ePlayer eMachine 

hover :: [Element] -> Behavior Game -> UI [Behavior Bool]
hover imgs state = mapM (stepper False) eHovers
  where
    hoverSquares = zipWith (\e s -> s <$ e) (UI.hover <$> imgs) squares
    leaves      = (fmap . fmap) (const False) (UI.leave <$> imgs)
    legal       = (\g -> isLegal (board g) (piece g)) <$> state
    hovering    = (\e -> legal <@> e) <$> hoverSquares
    eHovers     = zipWith union hovering leaves
           
----------------------------------------------------------------------
-- Build the GUI
----------------------------------------------------------------------

setup :: Window -> UI ()
setup window = void $ do
  return window # set title "Othello"

  let uiImg :: FilePath -> UI Element
      uiImg fp = UI.img # set UI.src fp
                        # set UI.style imgStyle

  imgs <- mapM uiImg initImgs
  let uiCells :: [UI Element]
      uiCells = map element imgs

  bState <- buildGameState imgs

  -- Update Board
  let setSrcs :: [FilePath] -> [UI Element] -> UI ()
      setSrcs fs es = zipWithM_ (set UI.src) fs es
  onChanges bState $ \g -> setSrcs (toUrls g) uiCells

  -- Display the winner
  let bNotify :: Behavior String
      bNotify = showNotification <$> bState
  notification <- UI.h2
  sink UI.text bNotify $ element notification

  -- Show legal moves when hovering over a cell.
  bHovers <- hover imgs bState
  let bHoverStyle :: [Behavior [(String, String)]]
      bHoverStyle = (fmap . fmap) showOpacity bHovers
  zipWithM_ (\b e -> sink UI.style b e) bHoverStyle uiCells
  
  getBody window #+ [ column
                      [ UI.h1 #+ [string "Othello"]
                      , grid (chunksOf 8 uiCells)
                      # set UI.style [("line-height", "0")]
                      , UI.div #+ [element notification]
                      # set UI.style [("color", "darkred")]
                      ]
                      # set UI.style colStyle
                    ]

----------------------------------------------------------------------
-- Styles
----------------------------------------------------------------------
type Style = [(String, String)]

imgStyle :: Style
imgStyle = [("width","50px"),("height","50px")]

btnStyle :: Style
btnStyle = [ ("font", "bold 24px Optima")
           , ("background-color", "#DDDDDD")
           , ("color", "darkred")
           , ("margin", "0 auto") ]

colStyle :: Style
colStyle = [ ("background-color","#DDDDDD")
           ,("text-align","center")
           ,("font-family","Optima, Arial, Helvetica, sans-serif")
           ,("margin","0 auto")
           ,("border","solid 3px #CACACA") ]
-------------------------------------------------------------------------
