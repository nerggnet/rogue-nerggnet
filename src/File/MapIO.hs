-- src/File/MapIO.hs
module File.MapIO (loadNewGame, loadSavedGame, saveGame) where

import File.Types
import qualified Game.Types as Game
import Game.State (updateVisibility, defaultFogRadius, charToTile)
import Game.GridUtils (updateTile)
import Data.Aeson (eitherDecode, encode, decodeFileStrict)
import qualified Data.ByteString.Lazy as B
import Linear.V2 (_x, _y)
import Control.Lens ((^.))
import Data.List (stripPrefix, nub)
import Data.Function ((&))
import Data.List.Extra (replace)

defaultWorldFile :: FilePath
defaultWorldFile = "world.json"

-- Load new game configuration
loadNewGame :: IO (Either GameConfig Game.GameState)
loadNewGame = do
  result <- loadMapLevels defaultWorldFile
  return $ case result of
    Left err -> error $ "Failed to load " ++ defaultWorldFile ++ ": " ++ err
    Right config -> Left config

loadSavedGame :: FilePath -> IO (Either String Game.GameState)
loadSavedGame saveFile = do
  rawState <- decodeFileStrict saveFile
  rawWorld <- decodeFileStrict defaultWorldFile
  case (rawState, rawWorld) of
    (Just state, Just worldConfig) ->
      return $ Right $ validateGameState $ recomputeVisibility $ restoreMapGrid (levels worldConfig) $ restoreGameState state
    _ -> error "Failed to load game state or world configuration"

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
saveGame savePath state = do
  let syncedState = syncSerializedTriggers state
      trimmedState = trimGameStateForSaving syncedState
      serializedState = encode trimmedState
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
    , Game.isRecurring = Game.triggerRecurring trigger
    }

sanitizeDescription :: String -> String
sanitizeDescription desc =
  desc
    -- Specific cleaning functions for known patterns
    & cleanPositionTrigger
    & cleanItemPickupTrigger
    & cleanTalkedToNpcTrigger
    & cleanPosAndItemsTrigger
    -- Generic replacements for "Just"
    & replace "Just (" "("
    & replace "Just \"" ""
    & replace "\"" ""
    & replace "Just [" "["
  where
    cleanPositionTrigger str =
      case stripPrefix "Position trigger at Just " str of
        Just rest -> "Position trigger at " ++ rest
        Nothing   -> str

    cleanItemPickupTrigger str =
      case stripPrefix "Item pickup trigger for Just " str of
        Just rest -> "Item pickup trigger for " ++ rest
        Nothing   -> str

    cleanTalkedToNpcTrigger str =
      case stripPrefix "Talked to NPC Just " str of
        Just rest -> "Talked to NPC " ++ rest
        Nothing   -> str

    cleanPosAndItemsTrigger str =
      case stripPrefix "Position and items trigger at Just " str of
        Just rest ->
          let (coordsPart, remaining) = break (== '[') rest
              coords = takeWhile (/= ' ') coordsPart
              itemsPart = drop 1 $ takeWhile (/= ']') remaining -- Extract the list of items
           in "Position and items trigger at " ++ coords ++ " requiring items: [" ++ itemsPart ++ "]"
        Nothing -> str

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
