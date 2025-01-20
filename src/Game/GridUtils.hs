-- src/Game/GridUtils.hs
module Game.GridUtils (updateTile) where

import Game.Types (Tile)

-- Update a single tile in the grid
updateTile :: [[Tile]] -> (Int, Int) -> Tile -> [[Tile]]
updateTile grid (x, y) newTile =
  let oldRow = grid !! y
      newRow = take x oldRow ++ [newTile] ++ drop (x + 1) oldRow
   in take y grid ++ [newRow] ++ drop (y + 1) grid
