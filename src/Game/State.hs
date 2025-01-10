-- src/Game/State.hs
{-# OPTIONS_GHC -Wno-x-partial #-}

module Game.State where

import Game.Types
import qualified File.Types as FT
import File.MapIO (loadMapLevels)
import Linear.V2 (V2(..))
import Data.Maybe (fromMaybe)

-- Default values for unarmed and unarmored player, plus default monster and fog radii
defaultHealth :: Int
defaultHealth = 100

defaultAttack :: Int
defaultAttack = 5

defaultResistance :: Int
defaultResistance = 3

defaultMonsterRadius :: Int
defaultMonsterRadius = 4

defaultFogRadius :: Int
defaultFogRadius = 5

-- Initialize the game state
initGame :: IO GameState
initGame = do
  fileMaps <- fromMaybe [] <$> loadMapLevels "world.json"
  let allWorlds = map transformFileWorld fileMaps
  let initialWorld = head allWorlds
  let initialState = GameState
        { player = Player
            { position = findStartingPosition initialWorld
            , health = defaultHealth
            , attack = defaultAttack
            , resistance = defaultResistance
            , inventory = []
            , equippedWeapon = Nothing
            , equippedArmor = Nothing }
        , levels = allWorlds
        , currentLevel = 0
        , message = ["Welcome Rogue nerggnet!", " ", " "]
        , commandBuffer = ""
        , commandMode = False
        , showLegend = False
        , gameOver = False
        }
  let updatedWorld = updateVisibility (player initialState) defaultFogRadius initialWorld
  let updatedState = initialState { levels = replaceLevel initialState 0 updatedWorld }
  return updatedState

-- Update what the player sees of the map
updateVisibility :: Player -> Int -> World -> World
updateVisibility plyr radius world =
  let pos = position plyr
      updatedVisibility = [ [manhattanDistance (V2 x y) pos <= radius | x <- [0..cols-1]] | y <- [0..rows-1] ]
      updatedDiscovered = zipWith (zipWith (||)) updatedVisibility (discovered world)
  in world { visibility = updatedVisibility, discovered = updatedDiscovered }
  where
    rows = length (mapGrid world)
    cols = length (head (mapGrid world))

-- Manhattan distance between two points
manhattanDistance :: V2 Int -> V2 Int -> Int
manhattanDistance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- Replace the current level with an updated one
replaceLevel :: GameState -> Int -> World -> [World]
replaceLevel state levelIndex newWorld =
  take levelIndex (levels state) ++ [newWorld] ++ drop (levelIndex + 1) (levels state)

-- Transform a File.Types.MapLevel to Game.Types.World
transformFileWorld :: FT.MapLevel -> World
transformFileWorld fileWorld = World
  { mapGrid = map (map charToTile) (FT.mapGrid fileWorld)
  , monsters = map transformMonster (FT.monsters fileWorld)
  , npcs = map transformNPC (FT.npcs fileWorld)
  , items = map transformItem (FT.items fileWorld)
  , doors = map transformDoorEntity (FT.doors fileWorld)
  , visibility = initializeGrid False (length $ FT.mapGrid fileWorld) (length $ head $ FT.mapGrid fileWorld)
  , discovered = initializeGrid False (length $ FT.mapGrid fileWorld) (length $ head $ FT.mapGrid fileWorld)
  }

initializeGrid :: a -> Int -> Int -> [[a]]
initializeGrid value rows cols = replicate rows (replicate cols value)

-- Transform a File.Types.JSONMonster to Game.Types.Monster
transformMonster :: FT.JSONMonster -> Monster
transformMonster fm = Monster
  { mPosition = uncurry V2 (FT.position fm)
  , mHealth = FT.health fm
  , mAttack = FT.attack fm
  , mName = FT.name fm
  }

-- Transform a File.Types.JSONNPC to Game.Types.NPC
transformNPC :: FT.JSONNPC -> NPC
transformNPC fnpc = NPC
  { npcName = FT.npcName fnpc
  , npcPosition = uncurry V2 (FT.npcPosition fnpc)
  , npcMessage = FT.npcMessage fnpc
  }

-- Transform a File.Types.JSONItem to Game.Types.Item
transformItem :: FT.JSONItem -> Item
transformItem fi = Item
  { iName = FT.itemName fi
  , iDescription = FT.itemDescription fi
  , iPosition = uncurry V2 (FT.itemPosition fi)
  , iCategory = case FT.itemCategory fi of
                  "Armor" -> Armor
                  "Weapon" -> Weapon
                  "Healing" -> Healing
                  "Special" -> Special
                  "Key" -> Key
                  _ -> error "Unknown category"
  , iEffectValue = FT.itemEffectValue fi
  }

-- Transform a File.Types.JSONDoorEntity to Game.Types.DoorEntity
transformDoorEntity :: FT.JSONDoorEntity -> DoorEntity
transformDoorEntity jsonDoor = DoorEntity
  { dePosition = uncurry V2 (FT.doorPosition jsonDoor)
  , deLocked   = FT.doorLocked jsonDoor
  }

-- Convert a character to a Tile
charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile '.' = Floor
charToTile '+' = Door
charToTile '<' = UpStair
charToTile '>' = DownStair
charToTile _   = Floor -- Default to Floor for unknown characters.

-- Find the starting position (e.g., the first Floor tile)
findStartingPosition :: World -> V2 Int
findStartingPosition wrld =
  let grid = mapGrid wrld
  in case [(x, y) | (y, row) <- zip [0..] grid, (x, tile) <- zip [0..] row, tile == Floor] of
       ((x, y):_) -> V2 x y
       _          -> V2 0 0 -- Default to top-left if no Floor tile is found.
