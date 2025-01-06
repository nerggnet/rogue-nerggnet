-- src/Game/State.hs
{-# OPTIONS_GHC -Wno-x-partial #-}

module Game.State where

import Game.Types
import qualified File.Types as FT
import File.MapIO (loadMapLevels)
import Linear.V2 (V2(..))
import Data.Maybe (fromMaybe)

-- Default values for unarmed and unarmored player, plus default monster attack (will be updated later)
defaultHealth :: Int
defaultHealth = 100

defaultAttack :: Int
defaultAttack = 5

defaultResistance :: Int
defaultResistance = 3

defaultMonsterAttack :: Int
defaultMonsterAttack = 8

-- Initialize the game state
initGame :: IO GameState
initGame = do
  fileMaps <- fromMaybe [] <$> loadMapLevels "world.json"
  let allWorlds = map transformFileWorld fileMaps
  let initialWorld = head allWorlds
  return GameState
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
    , message = ["Welcome to the roguelike game!", " ", " "]
    , commandBuffer = ""
    , commandMode = False
    }

-- Transform a File.Types.MapLevel to Game.Types.World
transformFileWorld :: FT.MapLevel -> World
transformFileWorld fileWorld = World
  { mapGrid = map (map charToTile) (FT.mapGrid fileWorld)
  , monsters = map transformMonster (FT.monsters fileWorld)
  , items = map transformItem (FT.items fileWorld)
  }

-- Transform a File.Types.JSONMonster to Game.Types.Monster
transformMonster :: FT.JSONMonster -> Monster
transformMonster fm = Monster
  { mPosition = uncurry V2 (FT.position fm) -- Convert (Int, Int) to V2 Int
  , mHealth = FT.health fm
  , mName = FT.name fm
  }

-- Transform a File.Types.JSONItem to Game.Types.Item
transformItem :: FT.JSONItem -> Item
transformItem fi = Item
  { iName = FT.itemName fi
  , iDescription = FT.itemDescription fi
  , iPosition = uncurry V2 (FT.itemPosition fi) -- Convert (Int, Int) to V2 Int
  , iCategory = case FT.itemCategory fi of
                  "Armor" -> Armor
                  "Weapon" -> Weapon
                  "Healing" -> Healing
                  "Special" -> Special
                  _ -> error "Unknown category"
  , iEffectValue = FT.itemEffectValue fi
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
