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
-- GUI
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
showOpacity b = if b then [("opacity", "0.8")] else [("opacity", "1")]

showNotification :: Game -> String
showNotification (Game p b)
  | isOver b = (show $ findWinner b) ++ " player wins!"
  | otherwise = show p ++ "'s turn"

setup :: Window -> UI ()
setup window = void $ do
  return window # set title "Othello"
  -- UI.addStyleSheet window "style.css"

  -- Create 64 empty tile images
  let uiImg :: FilePath -> UI Element
      uiImg fp = UI.img # set UI.src fp
                        # set UI.style [("width","50px"),("height","50px")]

  imgs <- mapM uiImg initImgs
  
  -- Turn our images into elements, and create events for each image
  let uiCells :: [UI Element]
      uiCells = map element imgs

      clicks :: [Event ()]
      clicks =  map UI.click imgs

      -- Create a stream of events
      moves :: Event Move
      moves = fmap concatenate . unions $ zipWith (\e s -> move s <$ e)
              clicks squares

  -- The Game state at the time of a click
  eState <- accumE newGame moves

  -- A behavior; a function from time t to Game
  bState <- stepper newGame eState

  -- Set Notification
  notification <- UI.h2

  let bNotify :: Behavior String
      bNotify = showNotification <$> bState

  sink UI.text bNotify $ element notification

  -- Show legal moves
  let hoverSquares :: [Event Square]
      hoverSquares = zipWith (\e s -> s <$ e) (UI.hover <$> imgs) squares

      leaves :: [Event Bool]
      leaves = (fmap . fmap) (const False) (UI.leave <$> imgs)

      bLegal :: Behavior (Square -> Bool)
      bLegal = (\g -> isLegal (board g) (piece g)) <$> bState

      eHovering :: [Event Bool]
      eHovering = (\e -> bLegal <@> e) <$> hoverSquares

      eHovers :: [Event Bool]
      eHovers = zipWith (unionWith (\a _ -> a)) eHovering leaves

  bHovers <- mapM (stepper False) eHovers

  let bHoverStyle :: [Behavior [(String, String)]]
      bHoverStyle = (fmap . fmap) showOpacity bHovers

  zipWithM_ (\b e -> sink UI.style b e) bHoverStyle uiCells
  
  -- Update Board
  let setSrcs :: [FilePath] -> [UI Element] -> UI ()
      setSrcs fs es = zipWithM_ (set UI.src) fs es
      
  onChanges bState $ \g -> do
    setSrcs (toUrls g) uiCells

  ai <- UI.button # set UI.text "AI Move"

  let eAIclick :: Event ()
      eAIclick = UI.click ai

      eAI :: Event Game
      eAI = bState <@ eAIclick

  bAI <- stepper "Hint" (nextMove <$> eAI)
  sink UI.text bAI (element ai)

  getBody window #+ [ column
                      [ UI.h1 #+ [string "Othello"]
                      , grid (chunksOf 8 uiCells) # set UI.style [("line-height", "0")]
                      , UI.div #+ [element notification] # set UI.class_ "notification"
                      , element ai # set UI.style [ ("font", "bold 24px Optima")
                                                  , ("background-color", "#DDDDDD")
                                                  , ("color", "darkred")
                                                  , ("margin", "0 auto") ]
                      ] # set UI.style [("background-color","#DDDDDD")
                                       ,("text-align","center")
                                       ,("font-family","Optima, Arial, Helvetica, sans-serif")
                                       ,("margin","0 auto")
                                       ,("border","solid 3px #CACACA")]
                    ]
