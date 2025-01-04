-- src/Game/State.hs
module Game.State where

import Game.Types
import Linear.V2 (V2(..))

-- Initialize the game state
initGame :: GameState
initGame = GameState
  { player   = Player { position = V2 25 13, health = 100, inventory = [] }
  , world    = generateMap 51 27
  , message       = ["Welcome to the roguelike game!", " ", " "]
  , commandBuffer = ""
  , commandMode   = False
  }

-- Generate a map of width x height
generateMap :: Int -> Int -> World
generateMap width height = World
  { mapGrid = [ [randomTile x y width height | x <- [0..width-1]] | y <- [0..height-1] ]
  , monsters = []
  , items = []
  }

-- Not random at all right now, but rather a tile generator
randomTile :: Int -> Int -> Int -> Int -> Tile
randomTile x y width height
  | x == 0 || y == 0 || x == width - 1 || y == height - 1 = Wall -- Borders are walls
  | x == 10 && y == 10 = Door
  | x == 15 && y == 15 = UpStair
  | x == 20 && y == 20 = DownStair
  | otherwise = Floor -- Everything else is floor
