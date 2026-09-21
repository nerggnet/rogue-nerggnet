-- src/UI/MainUI.hs
module UI.MainUI (startGame) where

import Brick
import Graphics.Vty
  ( Event(..), Key(..), rgbColor, withBackColor, withForeColor, defAttr
  , black, white, yellow, green, red, blue, magenta, cyan
  )
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import File.MapIO (defaultWorldFile, loadNewGame, loadSavedGame, persistGame)
import Game.State (maxHealth, newGame, treasureCarried)
import Game.Logic
import UI.Draw
import Game.Types
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.Random (initStdGen)
import System.IO (hPutStrLn, stderr)

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
  resumed <- if saveExists
    then loadSavedGame saveFile
    else pure (Left [])
  started <- case resumed of
    Right state -> pure (Right state)
    Left problems -> do
      -- An unreadable save is not fatal; it just means starting over.
      when saveExists $
        report ("Could not read " ++ saveFile ++ ", starting a new game") problems
      config <- loadNewGame
      gen <- initStdGen
      pure (config >>= newGame gen)

  case started of
    Left problems -> do
      report ("Could not start a game from " ++ defaultWorldFile) problems
      exitFailure
    Right initialState -> do
      finalState <- runGame initialState
      persistGame saveFile finalState
      putStrLn $ case (gameWon finalState, gameOver finalState) of
        (True, _) ->
          "You got out alive from level " ++ show (deepestLevel finalState + 1)
            ++ " with " ++ show (treasureCarried finalState)
            ++ " in treasure. Cleared the save, so next time starts a new dungeon."
        (_, True) ->
          "You died on level " ++ show (deepestLevel finalState + 1)
            ++ ", losing " ++ show (treasureCarried finalState)
            ++ " in treasure. Cleared the save, so next time starts a new dungeon."
        _ -> "Saving progress..."
  where
    report headline problems = do
      hPutStrLn stderr (headline ++ ":")
      mapM_ (hPutStrLn stderr . ("  - " ++)) problems

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
  config <- liftIO loadNewGame
  gen <- liftIO initStdGen
  case config >>= newGame gen of
    Right fresh -> put fresh
    -- The world file has changed since startup and no longer loads. Say so
    -- in the log rather than taking the running game down with it.
    Left problems -> modify $ \s -> s
      { message = ("Could not restart: " ++ intercalate "; " problems) : message s }
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
  , (attrName "shaft", withForeColor defAttr white)
  , (attrName "player", withForeColor defAttr blue)
  , (attrName "monster", withForeColor defAttr red)
  , (attrName "aimingMonster", withForeColor defAttr yellow)
  , (attrName "corpse", withForeColor defAttr red)
  , (attrName "npc", withForeColor defAttr cyan)
  , (attrName "item", withForeColor defAttr magenta)
  ]
