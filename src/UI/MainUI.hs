-- src/UI/MainUI.hs
module UI.MainUI (startGame) where

import Brick
import Graphics.Vty (Event(..))
import Graphics.Vty (rgbColor, withBackColor, withForeColor, defAttr, black, white, yellow, green, red, blue, magenta, cyan)
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import File.MapIO (loadNewGame, loadSavedGame, saveGame)
import Game.State (initGame)
import Game.Logic
import UI.Draw
import qualified Game.Types as Game
import System.Directory (doesFileExist)

saveFile :: FilePath
saveFile = "save.json"

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
  saveExists <- doesFileExist saveFile
  gameState <- if saveExists
    then do
      -- Load the saved game state
      savedGame <- loadSavedGame saveFile
      case savedGame of
        Left err -> do
          putStrLn $ "Failed to load " ++ saveFile ++ ": " ++ err
          -- Fallback to starting a new game
          loadNewGame
        Right state -> return $ Right state
    else loadNewGame

  finalState <- runGame $ initGame gameState
  saveGame saveFile finalState
  putStrLn "Game Over!"

runGame :: Game.GameState -> IO Game.GameState
runGame initialState = do
  let buildVty = mkVty defaultConfig
  vty <- buildVty
  finalState <- customMain vty buildVty Nothing app initialState
  return finalState

-- Handle events
handleEvent :: BrickEvent () e -> EventM () Game.GameState ()
handleEvent (VtyEvent (EvKey key [])) = do
  state <- get
  case Game.aimingState state of
    Just (Game.AimingState rangedItem) -> handleAimingInput key rangedItem -- Redirect to aiming logic
    Nothing -> do
      isCommandMode <- gets Game.commandMode
      modify $ \s -> s { Game.keyPressCount = (Game.keyPressCount s + 1) `mod` 3 }
      if isCommandMode
        then handleCommandInput key
        else handleMovement key
handleEvent _ = return ()

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap defAttr
  [ (attrName "fog", withBackColor defAttr black)
  , (attrName "discovered", withBackColor defAttr (rgbColor (40 :: Int) 40 40)) -- Dimly lit
  , (attrName "wall", withForeColor (withBackColor defAttr black) white)
  , (attrName "floor", withBackColor defAttr white)
  , (attrName "door", withForeColor defAttr yellow)
  , (attrName "upStair", withForeColor defAttr green)
  , (attrName "downStair", withForeColor defAttr green)
  , (attrName "player", withForeColor defAttr blue)
  , (attrName "monster", withForeColor defAttr red)
  , (attrName "npc", withForeColor defAttr cyan)
  , (attrName "item", withForeColor defAttr magenta)
  , (attrName "log", withForeColor defAttr yellow)
  ]
