-- src/Game/State.hs
{-# OPTIONS_GHC -Wno-x-partial #-}
module Game.State where

import Game.Types
import qualified File.Types as FT
import File.MapIO (loadMapLevels)
import Linear.V2 (V2(..))
import Data.Maybe (fromMaybe)

-- Initialize the game state
initGame :: IO GameState
initGame = do
  fileMaps <- fromMaybe [] <$> loadMapLevels "world.json" -- Handle missing files gracefully
  let initialWorld = transformFileWorld (head fileMaps) -- Use the first level as the initial world.
  return GameState
    { player = Player { position = findStartingPosition initialWorld, health = 100, inventory = [] }
    , world = initialWorld
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
