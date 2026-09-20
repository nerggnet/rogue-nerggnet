-- src/Game/GridUtils.hs
module Game.GridUtils (updateTile, gridLookup, orthogonal, keyedInventory) where

import Game.Types (Tile, Item)
import Linear.V2 (V2(..))
import Data.Maybe (catMaybes)

-- Update a single tile in the grid
updateTile :: [[Tile]] -> V2 Int -> Tile -> [[Tile]]
updateTile grid (V2 x y) newTile =
  let oldRow = grid !! y
      newRow = take x oldRow ++ [newTile] ++ drop (x + 1) oldRow
   in take y grid ++ [newRow] ++ drop (y + 1) grid

-- Look up a cell in a row-major grid, returning Nothing when out of bounds
gridLookup :: [[a]] -> V2 Int -> Maybe a
gridLookup grid (V2 x y)
  | x < 0 || y < 0 = Nothing
  | otherwise = case drop y grid of
      (row:_) -> case drop x row of
        (cell:_) -> Just cell
        []       -> Nothing
      [] -> Nothing

-- The four tiles sharing an edge with this one
orthogonal :: V2 Int -> [V2 Int]
orthogonal pos = [pos + V2 0 (-1), pos + V2 0 1, pos + V2 (-1) 0, pos + V2 1 0]

-- Generate a list of (key, item) pairs with equipped items on top
keyedInventory :: [Item] -> Maybe Item -> Maybe Item -> [(Char, Item)]
keyedInventory inv eqpdWeapon eqpdArmor =
  let equippedItems = catMaybes [eqpdWeapon, eqpdArmor]
      unequippedItems = filter (`notElem` equippedItems) inv
      prioritizedItems = equippedItems ++ unequippedItems
  in zip ['a'..] prioritizedItems

