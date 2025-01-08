-- src/UI/MainUI.hs
module UI.MainUI (startGame) where

import Brick
import Graphics.Vty (Event(..))
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import Game.State (initGame)
import Game.Logic
import UI.Draw
import qualified Game.Types as Game

-- App definition
app :: App Game.GameState e ()
app = App
  { appDraw = drawUI
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const defaultAttrMap
  , appChooseCursor = chooseCursor
  }

chooseCursor :: Game.GameState -> [CursorLocation n] -> Maybe (CursorLocation n)
chooseCursor state =
  if Game.commandMode state then showFirstCursor state else neverShowCursor state

-- Main function to start the game
startGame :: IO ()
startGame = do
  initialState <- initGame
  let buildVty = mkVty defaultConfig
  vty <- buildVty
  _ <- customMain vty buildVty Nothing app initialState
  putStrLn "Game Over!"

-- Handle events
handleEvent :: BrickEvent () e -> EventM () Game.GameState ()
handleEvent (VtyEvent (EvKey key [])) = do
  isCommandMode <- gets Game.commandMode
  if isCommandMode
    then handleCommandInput key
    else handleMovement key
handleEvent _ = return ()
