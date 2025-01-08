-- src/UI/Draw.hs
module UI.Draw (drawUI, keyedInventory) where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Game.Types
import Linear.V2 (V2(..))

-- Draw the UI
drawUI :: GameState -> [Widget ()]
drawUI state =
  [ drawLegendPopup | showLegend state ] ++
  [ vBox
      [ drawTitleBar
      , hBox
          [ padRight (Pad 2) $ drawMap currentWorld (player state)
          , padLeft (Pad 2) $
              vBox
                [ padTop (Pad 1) $ drawStatsBox (player state)
                , padTop (Pad 1) $ drawInventory (player state)
                ]
          ]
      , padTop (Pad 2) $ drawMessages updatedMessages
      , padTop (Pad 1) $ drawCommandInput state
      ]
  ]

  where
    currentWorld = levels state !! currentLevel state
    playerPos = position (player state)
    itemsOnPlayerTile = [iName item | item <- items currentWorld, iPosition item == playerPos]
    currentTileMessage =
      if null itemsOnPlayerTile then ""
        else "You see: " ++ unwords itemsOnPlayerTile

    -- Combine the tile-specific message with the general log
    updatedMessages = if null itemsOnPlayerTile then message state else currentTileMessage : message state

drawTitleBar :: Widget ()
drawTitleBar =
      padBottom (Pad 1) $ C.hCenter (str "Rogue-like Game (press ? for help)")

-- Draw the map
drawMap :: World -> Player -> Widget ()
drawMap wrld plyr =
  B.border $
    vBox $ zipWith drawRow [0..] (mapGrid wrld)
  where
    drawRow y row =
      hBox $ zipWith (\x tile -> drawTileWithFog wrld plyr x y tile) [0..] row

-- drawTileWithFog :: World -> Player -> Int -> Int -> Tile -> Widget ()
-- drawTileWithFog world plyr x y tile
--   | not (visibility world !! y !! x) && not (discovered world !! y !! x) = withAttr (attrName "fog") $ str " "
--   | not (visibility world !! y !! x) && discovered world !! y !! x = withAttr (attrName "discovered") $ drawTile tile
--   | position plyr == V2 x y = withAttr (attrName "player") $ str "@"
--   | any ((== V2 x y) . mPosition) (monsters world) = withAttr (attrName "monster") $ str "M"
--   | any ((== V2 x y) . iPosition) (items world) = withAttr (attrName "item") $ str "!"
--   | otherwise = case find (\d -> dePosition d == V2 x y) (doors world) of
--       Just door | deLocked door -> withAttr (attrName "door") $ str "D" -- Locked door
--       Just _ -> withAttr (attrName "door") $ str "+"                 -- Unlocked door
--       Nothing -> drawTile tile

drawTileWithFog :: World -> Player -> Int -> Int -> Tile -> Widget ()
drawTileWithFog world plyr x y tile
  | not (visibility world !! y !! x) && not (discovered world !! y !! x) = withAttr (attrName "fog") $ str " "
  | not (visibility world !! y !! x) && discovered world !! y !! x = withAttr (attrName "discovered") $ drawTile tile
  | position plyr == V2 x y = withAttr (attrName "player") $ str "@"
  | any ((== V2 x y) . mPosition) (monsters world) = withAttr (attrName "monster") $ str "M"
  | any ((== V2 x y) . iPosition) (items world) = withAttr (attrName "item") $ str "!"
  | otherwise = drawTile tile

-- Draw a single tile
drawTile :: Tile -> Widget ()
drawTile Wall      = str "#"
drawTile Floor     = str "."
drawTile Door      = withAttr (attrName "door") $ str "+"
drawTile UpStair   = withAttr (attrName "upStair") $ str "<"
drawTile DownStair = withAttr (attrName "downStair") $ str ">"

-- Draw the legend as a popup
drawLegendPopup :: Widget ()
drawLegendPopup =
  C.centerLayer $ -- Centered popup
    B.borderWithLabel (str "Commands") $
      padAll 1 $ vBox $ map str
        [ "Commands:"
        , "w - Move up"
        , "s - Move down"
        , "a - Move left"
        , "d - Move right"
        , "< - Ascend stairs/ladder"
        , "> - Descend stairs/ladder"
        , "g - Pick up item"
        , "u - Use an item from inventory"
        , ": - Enter command mode"
        , ":q - Quit the game"
        , "? - Toggle this help popup"
        ]

-- Draw the stats box with Health, Attack, and Resistance
drawStatsBox :: Player -> Widget ()
drawStatsBox plyr =
    hLimit 25 $
      B.borderWithLabel (str "Stats") $
        vBox
          [ padRight Max $ str $ "HP: " ++ show (health plyr)
          , padRight Max $ str $ "Attack: " ++ show (attack plyr)
          , padRight Max $ str $ "Resistance: " ++ show (resistance plyr)
          ]

-- Draw the inventory, highlighting equipped weapon and armor
drawInventory :: Player -> Widget ()
drawInventory plyr =
    hLimit 25 $
      B.borderWithLabel (str "Inventory") $
        padRight Max $
          if null inv
            then str "No items collected"
            else vBox $ map renderItem (keyedInventory inv eqpdWeapon eqpdArmor)
  where
    inv = inventory plyr
    eqpdWeapon = equippedWeapon plyr
    eqpdArmor = equippedArmor plyr

    renderItem (key, itm) =
      let equippedMarker
            | Just itm == eqpdWeapon = " (W)" -- Weapon marker
            | Just itm == eqpdArmor  = " (A)" -- Armor marker
            | otherwise                  = ""
      in str [key, ')', ' '] <+> str (iName itm ++ equippedMarker)

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

-- Draw messages/log
drawMessages :: [String] -> Widget ()
drawMessages msgs =
      vLimit 3 $ -- Limit to 3 rows
        vBox $ map str (reverse . take 3 $ msgs)

-- Draw the command input bar
drawCommandInput :: GameState -> Widget ()
drawCommandInput state =
  withBorderStyle BS.unicodeBold $
    hBox
      [ str "Command: "
      , showCursor () (Location (length (commandBuffer state), 0)) $
          str (commandBuffer state ++ " ")
      ]
