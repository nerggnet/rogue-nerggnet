-- src/Game/GridUtils.hs
module Game.GridUtils (updateTile, keyedInventory) where

import Game.Types (Tile, Item)

-- Update a single tile in the grid
updateTile :: [[Tile]] -> (Int, Int) -> Tile -> [[Tile]]
updateTile grid (x, y) newTile =
  let oldRow = grid !! y
      newRow = take x oldRow ++ [newTile] ++ drop (x + 1) oldRow
   in take y grid ++ [newRow] ++ drop (y + 1) grid

-- Generate a list of (key, item) pairs with equipped items on top
keyedInventory :: [Item] -> Maybe Item -> Maybe Item -> [(Char, Item)]
keyedInventory inv eqpdWeapon eqpdArmor =
  let equippedItems = concatMap maybeToList [eqpdWeapon, eqpdArmor]
      unequippedItems = filter (`notElem` equippedItems) inv
      prioritizedItems = equippedItems ++ unequippedItems
  in zip ['a'..] prioritizedItems
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

