-- src/File/MapIO.hs
module File.MapIO (loadNewGame, loadSavedGame, saveGame, persistGame, deleteSave) where

import qualified File.Types as FT
import Game.Types
import Game.State (updateVisibility, defaultFogRadius, charToTile)
import Game.GridUtils (updateTile)
import Data.Aeson (eitherDecode, eitherDecodeFileStrict, encode)
import qualified Data.ByteString.Lazy as B
import Data.List (nub)
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)

defaultWorldFile :: FilePath
defaultWorldFile = "world.json"

-- Load new game configuration
loadNewGame :: IO (Either FT.GameConfig GameState)
loadNewGame = do
  result <- loadMapLevels defaultWorldFile
  return $ case result of
    Left err -> error $ "Failed to load " ++ defaultWorldFile ++ ": " ++ err
    Right config -> Left config

-- A save file that cannot be read is reported rather than fatal, so that the
-- caller can fall back to starting a new game. Saves written by an older
-- version of the game fail here.
loadSavedGame :: FilePath -> IO (Either String GameState)
loadSavedGame saveFile = do
  rawState <- eitherDecodeFileStrict saveFile
  rawWorld <- eitherDecodeFileStrict defaultWorldFile
  return $ case (rawState, rawWorld) of
    (Left err, _) -> Left $ saveFile ++ ": " ++ err
    (_, Left err) -> Left $ defaultWorldFile ++ ": " ++ err
    (Right state, Right worldConfig) ->
      Right
        . validateGameState
        . recomputeVisibility
        $ restoreMapGrid (FT.levels worldConfig) state

validateGameState :: GameState -> GameState
validateGameState state
  | null (levels state) = error "No levels found in GameState!"
  | currentLevel state >= length (levels state) = error "currentLevel index out of bounds!"
  | otherwise = state

-- Load map FT.levels from a JSON file
loadMapLevels :: FilePath -> IO (Either String FT.GameConfig)
loadMapLevels path = do
  content <- B.readFile path
  return $ eitherDecode content

-- Save the current game state to a file
saveGame :: FilePath -> GameState -> IO ()
saveGame savePath state =
  B.writeFile savePath (encode (trimGameStateForSaving state))

-- Persist the game at the end of a session.
--
-- A finished run must not leave a save behind. Writing one would drop the
-- player straight back into the game over screen every time they started the
-- game, and leaving an older one in place would let them undo the death by
-- quitting. Either way the run is over, so the save goes.
persistGame :: FilePath -> GameState -> IO ()
persistGame savePath state
  | gameOver state || gameWon state = deleteSave savePath
  | otherwise                       = saveGame savePath state

-- Remove a save file, if there is one
deleteSave :: FilePath -> IO ()
deleteSave savePath = do
  exists <- doesFileExist savePath
  when exists (removeFile savePath)

-- Before saving, trim unnecessary fields like visibility
trimWorldForSaving :: World -> World
trimWorldForSaving world =
  let dscvrdCoords = nub $ gridToCoords (discovered world)
   in world { mapGrid = []
            , visibility = []
            , discovered = []
            , discoveredCoords = dscvrdCoords }

trimGameStateForSaving :: GameState -> GameState
trimGameStateForSaving state =
  state { levels = map trimWorldForSaving (levels state) }

-- Recompute visibility on load
recomputeVisibility :: GameState -> GameState
recomputeVisibility state =
  state { levels = zipWith updateVisibilityForLevel (levels state) [0 ..] }
  where
    updateVisibilityForLevel world levelIdx
      | levelIdx == currentLevel state =
          updateVisibility (player state) defaultFogRadius world
      | otherwise = world

-- Reload the map grid from the original world.json configuration, and apply tile overrides from ShiftTile actions
restoreMapGrid :: [FT.MapLevel] -> GameState -> GameState
restoreMapGrid mapLevels state =
  state { levels = zipWith restoreLevel mapLevels (levels state) }
  where
    restoreLevel mapLevel world =
      let baseGrid = map (map charToTile) (FT.mapGrid mapLevel)
          overriddenGrid = applyOverrides baseGrid (tileOverrides world)
       in world { mapGrid = overriddenGrid }

    applyOverrides = foldl (\g (pos, tile) -> updateTile g pos tile)
