{-# LANGUAGE TupleSections #-}
-- src/UI/MainUI.hs
module UI.MainUI (startGame, verifyReplay, defaultAttrMap) where

import Brick
import Graphics.Vty
  ( Event(..), Key(..), rgbColor, withBackColor, withForeColor, withStyle, defAttr, dim, reverseVideo
  , black, white, yellow, green, red, blue, magenta, cyan
  )
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import Data.Either (fromRight)
import File.Replays
import File.Scores
import File.MapIO (defaultWorldFile, loadNewGame, loadSavedGame, persistGame)
import Game.Replay
import Game.Score (runOf, runScore)
import Game.State (newGame, treasureCarried)
import Game.Logic
import UI.Draw
import Game.Types
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.Random (initStdGen, mkStdGen, uniformR)
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
    -- A resumed game is somebody else's run as far as a recording goes: the
    -- keys that got it here are gone. It plays, and it is scored, and it is
    -- not written down.
    Right state -> pure (Right (0, state))
    Left problems -> do
      -- An unreadable save is not fatal; it just means starting over.
      when saveExists $
        report ("Could not read " ++ saveFile ++ ", starting a new game") problems
      config <- loadNewGame
      -- The seed is drawn and kept, rather than taken from a generator and
      -- forgotten: a run cannot be written down without the number it
      -- started from.
      seed <- fst . uniformR (0, maxBound :: Int) <$> initStdGen
      pure (fmap (seed,) (config >>= newGame (mkStdGen seed)))

  -- A scoreboard that will not parse is reported and then ignored. Losing
  -- the history is not a reason to refuse to play.
  board <- loadScores defaultScoresFile
  case board of
    Left err -> report "Could not read the scoreboard, starting an empty one" [err]
    Right _  -> pure ()

  case started of
    Left problems -> do
      report ("Could not start a game from " ++ defaultWorldFile) problems
      exitFailure
    Right (seed, loaded) -> do
      let initialState = loaded {scoreboard = fromRight [] board}
      digest <- worldDigest
      finalState <- runGame initialState
      persistGame saveFile finalState
      stamped <- timestampNow
      placed <- recordRun defaultScoresFile stamped finalState
      filmed <- if seed == 0 then pure Nothing
                  else saveReplay defaultReplayDir digest seed stamped finalState
      let watchable = maybe "" (\p -> " Recorded to " ++ p ++ ".") filmed
          standing = case placed of
            Just (_, place, outOf) | place > 0 ->
              " Placed " ++ show place ++ " of " ++ show outOf ++ " in " ++ defaultScoresFile ++ "."
            Just _ -> " The scoreboard could not be written."
            Nothing -> ""
      putStrLn $ (++ (standing ++ watchable)) $ case (gameWon finalState, gameOver finalState) of
        (True, _) ->
          "You got out alive from floor " ++ show (deepestLevel finalState + 1)
            ++ " with " ++ show (treasureCarried finalState)
            ++ " in treasure. Cleared the save, so next time starts a new dungeon."
        (_, True) ->
          "You died on floor " ++ show (deepestLevel finalState + 1)
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

-- | Play a recorded run against the dungeon as it stands, and say whether
-- it comes out as it was written down.
--
-- This is what makes a score worth comparing: the scoreboard says somebody
-- carried thirteen thousand out of floor 12, and anybody with the same
-- dungeon can check it rather than take their word.
verifyReplay :: FilePath -> IO ()
verifyReplay path = do
  loaded <- loadReplay path
  case loaded of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right rec -> do
      digest <- worldDigest
      config <- loadNewGame
      case config of
        Left problems -> report ("Could not read " ++ defaultWorldFile) problems >> exitFailure
        Right cfg -> case replay digest cfg rec of
          Right ended -> case runOf (replayKeys rec `seq` "replay") ended of
            Nothing -> putStrLn "The run does not end; the keys run out first."
            Just r -> putStrLn $
              "Verified. " ++ show (runEnding r) ++ " on floor " ++ show (runDepth r)
                ++ " with " ++ show (runTreasure r) ++ " in treasure, "
                ++ show (runTurns r) ++ " turns, scoring " ++ show (runScore r)
                ++ " (seed " ++ show (replaySeed rec)
                ++ ", " ++ show (length (replayKeys rec)) ++ " keys)."
          Left (WrongDungeon written _) -> do
            hPutStrLn stderr
              ("This run was played on a different world.json (it wants "
               ++ written ++ ", this one is " ++ digest ++ ").")
            exitFailure
          Left (WouldNotStart problems) ->
            report ("Could not start a game from " ++ defaultWorldFile) problems >> exitFailure
          Left (EndedDifferently written got) -> do
            hPutStrLn stderr "The run does not come out as recorded."
            hPutStrLn stderr ("  recorded: " ++ show written)
            hPutStrLn stderr ("  replayed: " ++ show got)
            exitFailure
  where
    report headline problems = do
      hPutStrLn stderr (headline ++ ":")
      mapM_ (hPutStrLn stderr . ("  - " ++)) problems

-- | The character a replay records for a key, and that applyKey reads back.
--
-- Escape, Enter and Backspace become control characters a terminal would
-- never send as an ordinary key, so a run is a plain string of them.
keyChar :: Key -> Maybe Char
keyChar key = case key of
  KChar c -> Just c
  KEsc    -> Just '\ESC'
  KEnter  -> Just '\n'
  KBS     -> Just '\b'
  _       -> Nothing

-- Handle events
--
-- Every key goes through applyKey, which is where what a key means is
-- written down once, and is then written to the run being recorded. What
-- comes back still wanting a command is one of the two the state cannot
-- answer on its own.
handleEvent :: BrickEvent () e -> EventM () GameState ()
handleEvent (VtyEvent (EvKey key [])) =
  case keyChar key of
    Nothing -> return ()
    Just c -> do
      modify (\s -> applyKey c s {keysPressed = c : keysPressed s})
      wanted <- gets commandToExecute
      when wanted $ do
        cmd <- gets commandBuffer
        executeCommand cmd
        modify (\s -> s {commandToExecute = False, commandBuffer = ""})
handleEvent _ = return ()

-- The two commands the game itself has to answer. Everything else is in
-- applyCommand, where a replay can reach it.
executeCommand :: String -> EventM () GameState ()
executeCommand ":q" = halt -- Quit the game
executeCommand ":restart" = do -- Restart the game
  config <- liftIO loadNewGame
  gen <- liftIO initStdGen
  case config >>= newGame gen of
    -- A restart is a new run, so the keys recorded so far are not part of
    -- it any more.
    Right fresh -> put fresh
    -- The world file has changed since startup and no longer loads. Say so
    -- in the log rather than taking the running game down with it.
    Left problems -> modify $ \s -> s
      { message = ("Could not restart: " ++ intercalate "; " problems) : message s }
executeCommand _ = return ()  -- applyCommand has already dealt with it

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap defAttr
  [ (attrName "fog", withBackColor defAttr black)
    -- Remembered ground: seen once, not seen now. The shade carries it on a
    -- terminal with colours to spare, and the dim style carries it on one
    -- without -- vty drops a colour it cannot render, and that shade was
    -- the only thing telling remembered ground from lit.
  , (attrName "discovered",
      withStyle (withBackColor defAttr (rgbColor (40 :: Int) 40 40)) dim)
  , (attrName "door", withForeColor defAttr yellow)
  , (attrName "upStair", withForeColor defAttr green)
  , (attrName "downStair", withForeColor defAttr green)
  , (attrName "shaft", withForeColor defAttr white)
  , (attrName "player", withForeColor defAttr blue)
  , (attrName "monster", withForeColor defAttr red)
  , (attrName "shooter", withForeColor defAttr magenta)
  , (attrName "aimingMonster", withForeColor defAttr yellow)
  , (attrName "corpse", withForeColor defAttr red)
  , (attrName "sprung", withForeColor defAttr red)
  , (attrName "hurt", withStyle (withForeColor defAttr red) reverseVideo)
  , (attrName "npc", withForeColor defAttr cyan)
  , (attrName "item", withForeColor defAttr magenta)
  ]
