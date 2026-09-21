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
import Game.Types
import Game.State (helpPages, maxInventorySize, treasureCarried, visibleLogMessages, visibleMonsters, currentWorld)
import Game.GridUtils (keyedInventory)
import Linear.V2 (V2(..))
import Data.List (zip4)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Draw the UI
drawUI :: GameState -> [Widget ()]
drawUI state =
  [ drawLegendPopup (legendPage state) | legendPage state > 0 ] ++
  [ drawInventoryPopup mode (player state) | Just mode <- [inventoryMode state] ] ++
  [ drawVictoryScreen state | gameWon state ] ++
  [ drawGameOverScreen state | gameOver state && not (gameWon state) ] ++
  [ vBox
      [ drawTitleBar state
      , hBox
          [ padRight (Pad 2) $ drawMap world (player state) (aimingState state)
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
    world = currentWorld state
    playerPos = state.player.position
    itemsOnPlayerTile = [iName item | item <- items world, iPosition item == playerPos, not (iInactive item)]
    currentTileMessage =
      if null itemsOnPlayerTile then ""
        else "You see: " ++ unwords itemsOnPlayerTile

    -- Combine the tile-specific message with the general log
    updatedMessages = if null itemsOnPlayerTile then message state else currentTileMessage : message state

-- The stats box calls the player's experience level "Level", so depth is
-- named "Floor" here and never abbreviated, and says how far down the
-- dungeon goes so the number means something on its own.
drawTitleBar :: GameState -> Widget ()
drawTitleBar state =
      padBottom (Pad 1) $ C.hCenter (str title)
  where
    title = "Rogue nerggnet - Floor " ++ show (currentLevel state + 1)
              ++ " of " ++ show (length (levels state))
              ++ " (press ? for help)"

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
--
-- The grid goes in a viewport so that the screen fits the terminal whatever
-- size the dungeon is. Without one, a map taller than the window pushes the
-- message log and the command prompt off the bottom, where they are silently
-- cut off rather than scrolled to. The player's own tile is marked visible,
-- so the viewport follows them around a map bigger than the window.
--
-- A viewport takes all the room it is offered, which on a terminal larger
-- than the dungeon drew a border standing well clear of the map on the
-- bottom and the right. The limits inside cut the border back to the size of
-- the grid, and only ever take room away, so a window too small for the
-- level still gets the scrolling viewport it had before.
--
-- The padding outside them is what keeps that true. A limited widget is a
-- fixed-size one, and vBox hands fixed-size children their room before
-- anything else; the map would then have taken its 27 rows off the top of a
-- 24-row terminal and left the log and the command prompt with none. Padding
-- to Max makes it greedy again, so the panes around it are still measured
-- first and the map takes what is left -- it simply no longer draws a border
-- around the empty part of it.
drawMap :: World -> Player -> Maybe AimingState -> Widget ()
drawMap wrld plyr amngState =
  padRight Max $
    padBottom Max $
      B.border $
        vLimit (length grid) $
          hLimit (maximum (0 : map length grid)) $
            viewport () Both $
              vBox $ zipWith3 drawRow [0..] grid (zip (visibility wrld) (discovered wrld))
  where
    grid = mapGrid wrld
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
      visible $ withAttr (attrName "player") $ str "@"
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
drawTileHidden Shaft     = str "^"  -- A shaft stays worth remembering

-- Draw a single tile
drawTile :: Tile -> Widget ()
drawTile Wall      = str "#"
drawTile Floor     = str "."
drawTile Door      = withAttr (attrName "door") $ str "+"
drawTile UpStair   = withAttr (attrName "upStair") $ str "<"
drawTile DownStair = withAttr (attrName "downStair") $ str ">"
drawTile Start     = str "."
drawTile Shaft     = withAttr (attrName "shaft") $ str "^"

-- How the run went: how deep it got, and what it was worth.
--
-- The same two numbers either way, so that one attempt can be set against
-- another. Getting out is what turns treasure carried into treasure kept.
runSummary :: GameState -> [String]
runSummary state =
  [ "Reached level " ++ show (deepestLevel state + 1) ++ " of " ++ show (length (levels state))
  , "Treasure " ++ (if gameWon state then "carried out" else "lost") ++ ": "
      ++ show (treasureCarried state)
  , "Experience: " ++ show (xp (player state))
  ]

-- Draw the victory screen as a popup
drawVictoryScreen :: GameState -> Widget ()
drawVictoryScreen state =
  C.centerLayer $
    B.borderWithLabel (str "Victory") $
      padAll 2 $ vBox $
        [ C.hCenter $ str "You got out alive."
        , C.hCenter $ str " "
        ]
          ++ map (C.hCenter . str) (runSummary state)
          ++ [C.hCenter $ str " ", C.hCenter $ str "Press :q to exit."]

-- Draw the death screen as a popup.
--
-- Winning has always had one. Dying only wrote a line to the log, which
-- scrolls away, so it was easy to miss why the keys had stopped working.
drawGameOverScreen :: GameState -> Widget ()
drawGameOverScreen state =
  C.centerLayer $
    B.borderWithLabel (str "Game Over") $
      padAll 2 $ vBox $
        [ C.hCenter $ str "You have died."
        , C.hCenter $ str " "
        ]
          ++ map (C.hCenter . str) (runSummary state)
          ++ [ C.hCenter $ str " "
             , C.hCenter $ str "Press :restart for a new dungeon, or :q to quit."
             ]

-- Draw the item chooser as a popup.
--
-- The sidebar cannot list a full inventory on a short terminal, and the keys
-- are exactly what the player is about to press, so they are shown centred
-- while a choice is pending.
drawInventoryPopup :: InventoryMode -> Player -> Widget ()
drawInventoryPopup mode plyr =
  C.centerLayer $
    B.borderWithLabel (str title) $
      padAll 1 $
        -- Wide enough for the title too, or the border label is clipped.
        hLimit width $ padRight Max $ vBox (map str entries)
  where
    title = case mode of
      UseMode  -> "Use which item?"
      DropMode -> "Drop which item?"
    inv = inventory plyr
    eqpdWeapon = equippedWeapon plyr
    eqpdArmor = equippedArmor plyr
    entries
      | null inv = ["No items collected"]
      | otherwise =
          map (inventoryEntry eqpdWeapon eqpdArmor) (keyedInventory inv eqpdWeapon eqpdArmor)
    width = maximum (length title : map length entries)

-- Draw one page of the help as a popup
drawLegendPopup :: Int -> Widget ()
drawLegendPopup page =
  case drop (page - 1) helpPages of
    [] -> emptyWidget
    ((title, entries) : _) ->
      let label = title ++ " (" ++ show page ++ "/" ++ show (length helpPages) ++ ")"
          footer
            | page < length helpPages = "? for the next page"
            | otherwise = "? to close"
          body = entries ++ [" ", footer]
          -- Wide enough for the title too, or the border label is clipped.
          width = maximum (length label : map length body)
       in C.centerLayer $
            B.borderWithLabel (str label) $
              padAll 1 $ hLimit width $ padRight Max $ vBox (map str body)

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
          , padRight Max $ str $ "Treasure: " ++ show (sum (map iValue (inventory plyr)))
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

    renderItem = str . inventoryEntry eqpdWeapon eqpdArmor

-- One inventory line: the key that selects it, the name, the uses left and
-- whether it is equipped.
inventoryEntry :: Maybe Item -> Maybe Item -> (Char, Item) -> String
inventoryEntry eqpdWeapon eqpdArmor (key, itm) =
  [key, ')', ' '] ++ iName itm ++ usesText ++ equippedMarker
  where
    equippedMarker
      | Just itm == eqpdWeapon = " (W)" -- Weapon marker
      | Just itm == eqpdArmor  = " (A)" -- Armor marker
      | otherwise              = ""
    usesText = maybe "" (\uses -> " (" ++ show uses ++ ")") (iUses itm)

-- Draw messages/log
-- Draw the most recent log lines, oldest at the top.
--
-- Always exactly visibleLogMessages rows, blank ones included. The map takes
-- whatever vertical space the rest of the screen leaves, so a pane that grew
-- with the log would make the map shrink as messages arrived.
drawMessages :: [String] -> Widget ()
drawMessages msgs =
      vLimit visibleLogMessages $
        vBox $ map str (blanks ++ shown)
  where
    shown = reverse (take visibleLogMessages msgs)
    -- A space rather than "", which has no height to pad with.
    blanks = replicate (visibleLogMessages - length shown) " "

-- Draw the command input bar
--
-- vi's, rather than a labelled field: the line stays blank until ":" opens
-- it, and then shows the command exactly as typed, leading colon and all.
-- The label used to read "Command: ", which put a second colon on the screen
-- beside the one the player had just pressed. How to open the line is in the
-- help, under "?", where the commands themselves are listed.
--
-- The trailing space is what gives an empty line its height, and is where
-- the cursor sits while the buffer is empty.
drawCommandInput :: GameState -> Widget ()
drawCommandInput state =
  showCursor () (Location (length (commandBuffer state), 0)) $
    str (commandBuffer state ++ " ")
