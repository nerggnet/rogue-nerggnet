-- src/Game/State.hs
module Game.State where

import Game.Types
import Linear.V2 (V2(..))

-- Initialize the game state
initGame :: GameState
initGame = GameState
  { player   = Player { position = V2 5 5, health = 100, inventory = [] }
  , world    = generateMap 100 100
  , message       = ["Welcome to the roguelike game!", " ", " "]
  , commandBuffer = ""
  , commandMode   = False
  }

-- Generate a map with random features
generateMap :: Int -> Int -> World
generateMap width height = World
  { mapGrid = [ [randomTile x y | x <- [0..width-1]] | y <- [0..height-1] ]
  , monsters = []
  , items = []
  }

randomTile :: Int -> Int -> Tile
randomTile x y
  | x == 0 || y == 0 || x == width - 1 || y == height - 1 = Wall
  | x == 10 && y == 10 = Door
  | x == 15 && y == 15 = UpStair
  | x == 20 && y == 20 = DownStair
  | otherwise = let probability = (x * y) `mod` 10
                in if probability < 2 then Wall else Floor
  where
    width = 100
    height = 100
