-- src/File/MapIO.hs
module File.MapIO (loadNewGame, loadSavedGame, saveGame) where

import File.Types
import Game.Types (GameState)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as B

defaultWorldFile :: FilePath
defaultWorldFile = "world.json"

-- Load new game configuration
loadNewGame :: IO (Either GameConfig GameState)
loadNewGame = do
  result <- loadMapLevels defaultWorldFile
  return $ case result of
    Left err -> error $ "Failed to load " ++ defaultWorldFile ++ ": " ++ err
    Right config -> Left config

-- Load saved game state
loadSavedGame :: FilePath -> IO (Either String GameState)
loadSavedGame path = do
  content <- B.readFile path
  return $ eitherDecode content

-- Load map levels from a JSON file
loadMapLevels :: FilePath -> IO (Either String GameConfig)
loadMapLevels path = do
  content <- B.readFile path
  return $ eitherDecode content

-- Save the current game state to a file
saveGame :: FilePath -> GameState -> IO ()
saveGame savePath state = do
  let serializedState = encode state
  B.writeFile savePath serializedState
