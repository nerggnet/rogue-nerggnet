-- src/UI/MainUI.hs
module UI.MainUI (startGame) where

import Brick
import Graphics.Vty (Event(..), Key(..))
import Graphics.Vty (rgbColor, withBackColor, withForeColor, defAttr, black, white, yellow, green, red, blue, magenta, cyan)
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import File.MapIO (loadNewGame, loadSavedGame, saveGame)
import Game.State (initGame)
import Game.Logic
import UI.Draw
import qualified Game.Types as Game
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
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
chooseCursor state crsrs
  | Game.commandMode state || Game.aimingState state /= Nothing = showFirstCursor state crsrs
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
  putStrLn $ if Game.gameOver finalState then "Game Over!" else "Saving progress..."

runGame :: Game.GameState -> IO Game.GameState
runGame initialState = do
  let buildVty = mkVty defaultConfig
  vty <- buildVty
  finalState <- customMain vty buildVty Nothing app initialState
  return finalState

-- Handle events
handleEvent :: BrickEvent () e -> EventM () Game.GameState ()
handleEvent (VtyEvent (EvKey key [])) = do
  isCommandMode <- gets Game.commandMode
  modify $ \s -> s { Game.keyPressCount = (Game.keyPressCount s + 1) `mod` 3 }
  if isCommandMode
  then handleCommandInput key
  else handleMovement key
handleEvent _ = return ()

-- Handle movement keys
handleMovement :: Key -> EventM () Game.GameState ()
handleMovement key = do
  let keyChar = case key of
        KChar c -> Just c
        _       -> Nothing
  modify (handleMovementInternal keyChar)

handleCommandInput :: Key -> EventM () Game.GameState ()
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
  when (Game.commandToExecute newState) $ do
    executeCommand (Game.commandBuffer newState)
    modify (\s -> s { Game.commandToExecute = False, Game.commandBuffer = "" })  -- Clear buffer after execution

-- Execute commands
executeCommand :: String -> EventM () Game.GameState ()
executeCommand ":q" = halt -- Quit the game
executeCommand ":restart" = do -- Restart the game
  newState <- liftIO loadNewGame
  case newState of
    Left _ -> put $ initGame newState -- $ Left (config { Game.message = ["Game restarted!"] } )
    Right state -> put state
executeCommand ":heal" = do -- Cheat
    state <- get
    let plyr = Game.player state
        playerCurrentMaxHealth = Game.xpHealth (Game.xpLevels state !! (Game.playerXPLevel plyr - 1))
    modify (\s -> s { Game.player = plyr { Game.health = playerCurrentMaxHealth }, Game.gameOver = False, Game.commandToExecute = False } )
executeCommand ":super" = do -- Cheat a lot
    state <- get
    let plyr = Game.player state
    modify (\s -> s { Game.player = plyr { Game.health = 1000, Game.attack = 100, Game.resistance = 100 }, Game.gameOver = False, Game.commandToExecute = False } )
executeCommand cmd  = modify (\s -> s { Game.message = ("Unknown command: " ++ cmd) : Game.message s, Game.commandToExecute = False })

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
  , (attrName "monster", withForeColor defAttr red)
  , (attrName "aimingMonster", withForeColor defAttr yellow)
  , (attrName "npc", withForeColor defAttr cyan)
  , (attrName "item", withForeColor defAttr magenta)
  , (attrName "log", withForeColor defAttr yellow)
  ]
