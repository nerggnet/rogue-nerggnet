-- src/File/MapIO.hs
module File.MapIO (loadNewGame, loadSavedGame, saveGame) where

import File.Types
import qualified Game.Types as Game
import Game.State (updateVisibility, defaultFogRadius, charToTile)
import Game.GridUtils (updateTile)
import Data.Aeson (eitherDecode, eitherDecodeFileStrict, encode)
import qualified Data.ByteString.Lazy as B
import Linear.V2 (_x, _y)
import Control.Lens ((^.))
import Data.List (nub)

defaultWorldFile :: FilePath
defaultWorldFile = "world.json"

-- Load new game configuration
loadNewGame :: IO (Either GameConfig Game.GameState)
loadNewGame = do
  result <- loadMapLevels defaultWorldFile
  return $ case result of
    Left err -> error $ "Failed to load " ++ defaultWorldFile ++ ": " ++ err
    Right config -> Left config

-- A save file that cannot be read is reported rather than fatal, so that the
-- caller can fall back to starting a new game. Saves written by an older
-- version of the game fail here.
loadSavedGame :: FilePath -> IO (Either String Game.GameState)
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
        . restoreMapGrid (levels worldConfig)
        $ restoreGameState state

validateGameState :: Game.GameState -> Game.GameState
validateGameState state
  | null (Game.levels state) = error "No levels found in GameState!"
  | Game.currentLevel state >= length (Game.levels state) = error "currentLevel index out of bounds!"
  | otherwise = state

-- Load map levels from a JSON file
loadMapLevels :: FilePath -> IO (Either String GameConfig)
loadMapLevels path = do
  content <- B.readFile path
  return $ eitherDecode content

-- Save the current game state to a file
saveGame :: FilePath -> Game.GameState -> IO ()
saveGame savePath state =
  B.writeFile savePath (encode (trimGameStateForSaving state))

-- Before saving, trim unnecessary fields like visibility
trimWorldForSaving :: Game.World -> Game.World
trimWorldForSaving world =
  let dscvrdCoords = nub $ Game.gridToCoords (Game.discovered world)
   in world { Game.mapGrid = []
            , Game.visibility = []
            , Game.discovered = []
            , Game.discoveredCoords = dscvrdCoords }

trimGameStateForSaving :: Game.GameState -> Game.GameState
trimGameStateForSaving state =
  state { Game.levels = map trimWorldForSaving (Game.levels state) }

-- Recompute visibility on load
recomputeVisibility :: Game.GameState -> Game.GameState
recomputeVisibility state =
  state { Game.levels = zipWith updateVisibilityForLevel (Game.levels state) [0 ..] }
  where
    updateVisibilityForLevel world levelIdx
      | levelIdx == Game.currentLevel state =
          updateVisibility (Game.player state) defaultFogRadius world
      | otherwise = world

-- Rebuild the discovered grid on load
restoreWorld :: Game.World -> Game.World
restoreWorld world =
    if length (Game.mapGrid world) > 0
    then
       let dscvrdCoords = Game.discoveredCoords world
           dscvrd = Game.coordsToGrid dscvrdCoords (Game.mapRows world) (Game.mapCols world)
        in world { Game.discovered = dscvrd }
    else world

restoreGameState :: Game.GameState -> Game.GameState
restoreGameState state =
  state { Game.levels = map restoreWorld (Game.levels state) }

-- Reload the map grid from the original world.json configuration, and apply tile overrides from ShiftTile actions
restoreMapGrid :: [MapLevel] -> Game.GameState -> Game.GameState
restoreMapGrid mapLevels state =
  state { Game.levels = zipWith restoreLevel mapLevels (Game.levels state) }
  where
    restoreLevel mapLevel world =
      let baseGrid = map (map charToTile) (mapGrid mapLevel)
          overriddenGrid = applyOverrides baseGrid (Game.tileOverrides world)
       in world { Game.mapGrid = overriddenGrid }

    applyOverrides grid overrides =
      foldl (\g (pos, tile) -> updateTile g (pos ^. _x, pos ^. _y) tile) grid overrides
