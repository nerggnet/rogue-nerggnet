-- src/UI/Draw.hs
module UI.Draw
  ( drawUI
    -- Exposed for testing: these are what rendering a tile actually consults.
  , MapView(..)
  , mapView
  ) where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Game.Types
import Game.State (maxInventorySize, visibleMonsters)
import Game.GridUtils (keyedInventory)
import Linear.V2 (V2(..))
import Data.List (zip4)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Draw the UI
drawUI :: GameState -> [Widget ()]
drawUI state =
  [ drawLegendPopup | showLegend state ] ++
  [ drawVictoryScreen | gameWon state ] ++
  [ vBox
      [ drawTitleBar
      , hBox
          [ padRight (Pad 2) $ drawMap currentWorld (player state) (aimingState state)
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
    itemsOnPlayerTile = [iName item | item <- items currentWorld, iPosition item == playerPos, not (iInactive item)]
    currentTileMessage =
      if null itemsOnPlayerTile then ""
        else "You see: " ++ unwords itemsOnPlayerTile

    -- Combine the tile-specific message with the general log
    updatedMessages = if null itemsOnPlayerTile then message state else currentTileMessage : message state

drawTitleBar :: Widget ()
drawTitleBar =
      padBottom (Pad 1) $ C.hCenter (str "Rogue nerggnet (press ? for help)")

-- | What a tile needs to know about the rest of the level.
--
-- Built once per frame. Drawing a tile used to rescan the monster, item and
-- NPC lists and index into the visibility grids, which made rendering cost
-- the size of the map times the number of entities on it.
data MapView = MapView
  { viewPlayer   :: V2 Int            -- Where the player is standing
  , viewAiming   :: Bool              -- Are we picking a ranged target?
  , viewLetters  :: Map.Map (V2 Int) Char -- Targeting letters for visible monsters
  , viewMonsters :: Set.Set (V2 Int)  -- Active monsters
  , viewItems    :: Set.Set (V2 Int)  -- Items that are on the floor and visible
  , viewNpcs     :: Set.Set (V2 Int)
  , viewCorpses  :: Set.Set (V2 Int)
  }

mapView :: World -> Player -> Maybe AimingState -> MapView
mapView world plyr amngState =
  MapView
    { viewPlayer   = position plyr
    , viewAiming   = isJust amngState
      -- Shared with the ranged-targeting logic so the letters always agree.
    , viewLetters  = Map.fromList [(mPosition m, c) | (c, m) <- visibleMonsters world]
    , viewMonsters = Set.fromList (map mPosition (filter (not . mInactive) (monsters world)))
    , viewItems    = Set.fromList
        [iPosition i | i <- items world, not (iHidden i), not (iInactive i)]
    , viewNpcs     = Set.fromList (map npcPosition (npcs world))
    , viewCorpses  = Set.fromList (corpses world)
    }

-- Draw the map
drawMap :: World -> Player -> Maybe AimingState -> Widget ()
drawMap wrld plyr amngState =
  B.border $
    vBox $ zipWith3 drawRow [0..] (mapGrid wrld) (zip (visibility wrld) (discovered wrld))
  where
    view = mapView wrld plyr amngState
    drawRow y tiles (visRow, seenRow) =
      hBox [ drawTileWithFog view (V2 x y) tile vis seen
           | (x, tile, vis, seen) <- zip4 [0..] tiles visRow seenRow ]

drawTileWithFog :: MapView -> V2 Int -> Tile -> Bool -> Bool -> Widget ()
drawTileWithFog view pos tile lit seen
  | not lit && not seen =
      withAttr (attrName "fog") $ str " "
  | not lit =
      withAttr (attrName "discovered") $ drawTileHidden tile
  | viewPlayer view == pos =
      withAttr (attrName "player") $ str "@"
  | viewAiming view
  , Just monsterChar <- Map.lookup pos (viewLetters view) =
      withAttr (attrName "aimingMonster") $ str [monsterChar]
  | Set.member pos (viewMonsters view) =
      withAttr (attrName "monster") $ str "M"
  | Set.member pos (viewItems view) =
      withAttr (attrName "item") $ str "!"
  | Set.member pos (viewNpcs view) =
      withAttr (attrName "npc") $ str "N"
  | Set.member pos (viewCorpses view) =
      withAttr (attrName "corpse") $ str "†"
  | otherwise =
      drawTile tile

-- Helper to render a hidden tile (e.g., in fog or discovered but not visible)
drawTileHidden :: Tile -> Widget ()
drawTileHidden Wall      = str "#"
drawTileHidden Floor     = str "."  -- Use generic appearance for hidden tiles
drawTileHidden Door      = str "."  -- Doors appear as regular floor when hidden
drawTileHidden UpStair   = str "."  -- Up stairs appear as regular floor when hidden
drawTileHidden DownStair = str "."  -- Down stairs appear as regular floor when hidden
drawTileHidden Start     = str "."  -- Starting position

-- Draw a single tile
drawTile :: Tile -> Widget ()
drawTile Wall      = str "#"
drawTile Floor     = str "."
drawTile Door      = withAttr (attrName "door") $ str "+"
drawTile UpStair   = withAttr (attrName "upStair") $ str "<"
drawTile DownStair = withAttr (attrName "downStair") $ str ">"
drawTile Start     = str "."

-- Draw the victory screen as a popup
drawVictoryScreen :: Widget ()
drawVictoryScreen =
  C.centerLayer $
    B.borderWithLabel (str "Victory") $
      padAll 2 $ vBox
        [ C.hCenter $ str "Congratulations!"
        , C.hCenter $ str "You have won the game!"
        , C.hCenter $ str "Press :q to exit."
        ]

-- Draw the legend as a popup
drawLegendPopup :: Widget ()
drawLegendPopup =
  C.centerLayer $ -- Centered popup
    B.borderWithLabel (str "Commands") $
      padAll 1 $ vBox $ map str
        [ "Commands:"
        , "w or k - Move up"
        , "s or j - Move down"
        , "a or h - Move left"
        , "d or l - Move right"
        , "< - Ascend stairs/ladder"
        , "> - Descend stairs/ladder"
        , "g - Pick up item"
        , "u - Use an item from inventory"
        , "x - Drop an item from inventory"
        , ": - Enter command mode"
        , ":q - Quit the game"
        , ":restart - Restart the game"
        , "? - Toggle this help popup"
        ]

-- Draw the stats box with Health, Attack, and Resistance
drawStatsBox :: Player -> Widget ()
drawStatsBox plyr =
    hLimit 30 $
      B.borderWithLabel (str "Stats") $
        vBox
          [ padRight Max $ str $ "Level: " ++ show (playerXPLevel plyr)
          , padRight Max $ str $ "HP: " ++ show (health plyr)
          , padRight Max $ str $ "Attack: " ++ show (attack plyr) ++ " (Base: " ++ show (baseAttack plyr) ++ ")"
          , padRight Max $ str $ "Resistance: " ++ show (resistance plyr) ++ " (Base: " ++ show (baseResistance plyr) ++ ")"
          , padRight Max $ str $ "XP: " ++ show (xp plyr)
          ]

-- Draw the inventory, highlighting equipped weapon and armor
drawInventory :: Player -> Widget ()
drawInventory plyr =
    hLimit 30 $
      B.borderWithLabel (str $ "Inventory " ++ "(" ++ show inventorySize ++ "/" ++ show maxInventorySize ++ ")") $
        padRight Max $
          if null inv
            then str "No items collected"
            else vBox $ map renderItem (keyedInventory inv eqpdWeapon eqpdArmor)
  where
    inv = inventory plyr
    inventorySize = length inv
    eqpdWeapon = equippedWeapon plyr
    eqpdArmor = equippedArmor plyr

    renderItem (key, itm) =
      let equippedMarker
            | Just itm == eqpdWeapon = " (W)" -- Weapon marker
            | Just itm == eqpdArmor  = " (A)" -- Armor marker
            | otherwise              = ""
          usesText = case iUses itm of
                       Just uses -> " (" ++ show uses ++ ")"
                       Nothing   -> ""
      in str [key, ')', ' '] <+> str (iName itm ++ usesText ++ equippedMarker)

-- Draw messages/log
drawMessages :: [String] -> Widget ()
drawMessages msgs =
      vLimit 5 $ -- Limit to 3 rows
        vBox $ map str (reverse . take 5 $ msgs)

-- Draw the command input bar
drawCommandInput :: GameState -> Widget ()
drawCommandInput state =
  withBorderStyle BS.unicodeBold $
    hBox
      [ str "Command: "
      , showCursor () (Location (length (commandBuffer state), 0)) $
          str (commandBuffer state ++ " ")
      ]
