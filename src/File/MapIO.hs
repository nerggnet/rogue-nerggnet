-- src/File/MapIO.hs
module File.MapIO (loadNewGame, loadSavedGame, saveGame) where

import File.Types
import qualified Game.Types as Game
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import Data.List (stripPrefix)

defaultWorldFile :: FilePath
defaultWorldFile = "world.json"

-- Load new game configuration
loadNewGame :: IO (Either GameConfig Game.GameState)
loadNewGame = do
  result <- loadMapLevels defaultWorldFile
  return $ case result of
    Left err -> error $ "Failed to load " ++ defaultWorldFile ++ ": " ++ err
    Right config -> Left config

-- Load saved game state
loadSavedGame :: FilePath -> IO (Either String Game.GameState)
loadSavedGame path = do
  content <- B.readFile path
  return $ eitherDecode content

-- Load map levels from a JSON file
loadMapLevels :: FilePath -> IO (Either String GameConfig)
loadMapLevels path = do
  content <- B.readFile path
  return $ eitherDecode content

-- Save the current game state to a file
saveGame :: FilePath -> Game.GameState -> IO ()
saveGame savePath state = do
  let syncedState = syncSerializedTriggers state
      serializedState = encode syncedState
  B.writeFile savePath serializedState

-- Sync serializedTriggers with the remaining active triggers
syncSerializedTriggers :: Game.GameState -> Game.GameState
syncSerializedTriggers state =
  let updatedLevels = map syncLevel (Game.levels state)
   in state { Game.levels = updatedLevels }

syncLevel :: Game.World -> Game.World
syncLevel world =
  world { Game.serializedTriggers = map toSerializableTrigger (Game.triggers world) }

toSerializableTrigger :: Game.Trigger -> Game.SerializableTrigger
toSerializableTrigger trigger =
  Game.SerializableTrigger
    { Game.actions = Game.triggerActions trigger
    , Game.description = sanitizeDescription $ Game.triggerDescription trigger
    }

sanitizeDescription :: String -> String
sanitizeDescription desc =
  let removeJustPrefix str =
        case stripPrefix "Position trigger at Just " str of
          Just rest -> "Position trigger at " ++ rest
          Nothing   -> str
      cleanItemPickup str =
        case stripPrefix "Item pickup trigger for Just \"" str of
          Just rest -> "Item pickup trigger for \"" ++ takeWhile (/= '"') rest ++ "\""
          Nothing   -> str
      cleanTalkedToNPC str =
        case stripPrefix "Talked to NPC Just \"" str of
          Just rest -> "Talked to NPC \"" ++ takeWhile (/= '"') rest ++ "\""
          Nothing   -> str
  in cleanTalkedToNPC (cleanItemPickup (removeJustPrefix desc))
