-- src/UI/MainUI.hs
module UI.MainUI (startGame) where

import Brick
import Graphics.Vty
  ( Event(..), Key(..), rgbColor, withBackColor, withForeColor, defAttr
  , black, yellow, green, red, blue, magenta, cyan
  )
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import File.MapIO (loadNewGame, loadSavedGame, saveGame)
import Game.State (initGame, maxHealth)
import Game.Logic
import UI.Draw
import Game.Types
import Control.Monad (when)
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)

saveFile :: FilePath
saveFile = "save.json"

-- App definition
app :: App GameState e ()
app = App
  { appDraw = drawUI
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const defaultAttrMap
  , appChooseCursor = chooseCursor
  }

chooseCursor :: GameState -> [CursorLocation n] -> Maybe (CursorLocation n)
chooseCursor state crsrs
  | commandMode state || isJust (aimingState state) = showFirstCursor state crsrs
  | otherwise = neverShowCursor state crsrs

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
  putStrLn $ if gameOver finalState then "Game Over!" else "Saving progress..."

runGame :: GameState -> IO GameState
runGame initialState = do
  let buildVty = mkVty defaultConfig
  vty <- buildVty
  customMain vty buildVty Nothing app initialState

-- Handle events
handleEvent :: BrickEvent () e -> EventM () GameState ()
handleEvent (VtyEvent (EvKey key [])) = do
  isCommandMode <- gets commandMode
  if isCommandMode
  then handleCommandInput key
  else handleMovement key
handleEvent _ = return ()

-- Handle movement keys
handleMovement :: Key -> EventM () GameState ()
handleMovement key = do
  let keyChar = case key of
        KChar c -> Just c
        _       -> Nothing
  modify (handleMovementInternal keyChar)

handleCommandInput :: Key -> EventM () GameState ()
handleCommandInput key = do
  state <- get
  let (keyChar, escPressed) = case key of
        KChar c -> (Just c, False)
        KEsc    -> (Nothing, True)
        KEnter  -> (Just '\n', False)
        KBS     -> (Just '\b', False)
        _       -> (Nothing, False)
  let stateModifier = handleCommandInputInternal keyChar escPressed state
  modify stateModifier

  -- Check to see if there is a command to execute
  newState <- get
  when (commandToExecute newState) $ do
    executeCommand (commandBuffer newState)
    modify (\s -> s { commandToExecute = False, commandBuffer = "" })  -- Clear buffer after execution

-- Execute commands
executeCommand :: String -> EventM () GameState ()
executeCommand ":q" = halt -- Quit the game
executeCommand ":restart" = do -- Restart the game
  newState <- liftIO loadNewGame
  case newState of
    Left _ -> put $ initGame newState -- $ Left (config { message = ["Game restarted!"] } )
    Right state -> put state
executeCommand ":heal" = do -- Cheat
    state <- get
    let plyr = player state
    modify (\s -> s { player = plyr { health = maxHealth state }, gameOver = False, commandToExecute = False } )
executeCommand ":super" = do -- Cheat a lot
    state <- get
    let plyr = player state
    modify (\s -> s { player = plyr { health = 1000, attack = 100, resistance = 100 }, gameOver = False, commandToExecute = False } )
executeCommand cmd  = modify (\s -> s { message = ("Unknown command: " ++ cmd) : message s, commandToExecute = False })

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap defAttr
  [ (attrName "fog", withBackColor defAttr black)
  , (attrName "discovered", withBackColor defAttr (rgbColor (40 :: Int) 40 40)) -- Dimly lit
  , (attrName "door", withForeColor defAttr yellow)
  , (attrName "upStair", withForeColor defAttr green)
  , (attrName "downStair", withForeColor defAttr green)
  , (attrName "player", withForeColor defAttr blue)
  , (attrName "monster", withForeColor defAttr red)
  , (attrName "aimingMonster", withForeColor defAttr yellow)
  , (attrName "corpse", withForeColor defAttr red)
  , (attrName "npc", withForeColor defAttr cyan)
  , (attrName "item", withForeColor defAttr magenta)
  ]
