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
import Game.Score (ranked, runOf, runScore)
import Game.State (helpPages, loggedOnScreen, maxInventorySize, nextXPLevel, treasureCarried, visibleLogMessages, visibleMonsters, currentWorld, whatItDoes)
import Game.GridUtils (keyedInventory)
import Linear.V2 (V2(..))
import Data.List (intercalate, zip4)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Draw the UI
drawUI :: GameState -> [Widget ()]
drawUI state =
  [ drawScoresPopup state | showScores state ] ++
  [ drawLogPopup state | showLog state ] ++
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
                [ padTop (Pad 1) $ drawStatsBox state
                , padTop (Pad 1) $ drawInventory (player state)
                ]
          ]
      , padTop (Pad 1) $ drawMessages updatedMessages
      , drawCommandInput state
      ]
  ]

  where
    world = currentWorld state
    playerPos = state.player.position
    itemsUnderfoot = [item | item <- items world, iPosition item == playerPos, not (iInactive item)]
    -- One item gets the full treatment, since there is room for it. Several
    -- get their names, which is all that fits and all that is needed to
    -- decide which to stand on.
    currentTileMessage = case itemsUnderfoot of
      []    -> ""
      [itm] -> "You see: " ++ iName itm ++ " - " ++ whatItDoes itm
      many  -> "You see: " ++ intercalate ", " (map iName many)

    -- Combine the tile-specific message with the general log
    updatedMessages = if null itemsUnderfoot then message state else currentTileMessage : message state

-- The stats box calls the player's experience level "Level", so depth is
-- named "Floor" here and never abbreviated, and says how far down the
-- dungeon goes so the number means something on its own.
--
-- No blank line under it, and only one above the log rather than two. Three
-- rows of decoration is three rows of dungeon on a terminal that has not
-- got them to spare: this layout wanted 40 rows to show a floor whole, and
-- wants 37 now.
drawTitleBar :: GameState -> Widget ()
drawTitleBar state =
      C.hCenter (str title)
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
  , viewShooters :: Set.Set (V2 Int)  -- Of those, the ones that strike at range
  , viewItems    :: Set.Set (V2 Int)  -- Items that are on the floor and visible
  , viewNpcs     :: Set.Set (V2 Int)
  , viewCorpses  :: Set.Set (V2 Int)
  , viewSprung   :: Set.Set (V2 Int)  -- Where something in the floor went off
  , viewOpenDoors :: Set.Set (V2 Int) -- Doorways standing open
  , viewGraves   :: Set.Set (V2 Int)  -- Where earlier runs ended
  }

mapView :: World -> Player -> Maybe AimingState -> MapView
mapView world plyr amngState =
  MapView
    { viewPlayer   = position plyr
    , viewAiming   = isJust amngState
      -- Shared with the ranged-targeting logic so the letters always agree.
    , viewLetters  = Map.fromList [(mPosition m, c) | (c, m) <- visibleMonsters world]
    , viewMonsters = Set.fromList (map mPosition (filter (not . mInactive) (monsters world)))
    , viewShooters = Set.fromList
        [mPosition m | m <- monsters world, not (mInactive m), isJust (mRange m)]
    , viewItems    = Set.fromList
        [iPosition i | i <- items world, not (iHidden i), not (iInactive i)]
    , viewNpcs     = Set.fromList (map npcPosition (npcs world))
    , viewCorpses  = Set.fromList (corpses world)
    , viewSprung   = Set.fromList (sprung world)
    , viewOpenDoors = Set.fromList [dePosition d | d <- doors world, not (deBlocks d)]
    , viewGraves   = Set.fromList (map graveAt (graves world))
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
  -- Standing in one, the player covers it, so the player is what has to
  -- show it: a blade in the floor is worth seeing at the moment it goes
  -- off, and the log line saying so scrolls away.
  | viewPlayer view == pos, Set.member pos (viewSprung view) =
      visible $ withAttr (attrName "hurt") $ str "@"
  | viewPlayer view == pos =
      visible $ withAttr (attrName "player") $ str "@"
  | viewAiming view
  , Just monsterChar <- Map.lookup pos (viewLetters view) =
      withAttr (attrName "aimingMonster") $ str [monsterChar]
  -- A letter of its own rather than a shade of the same one: something that
  -- can hit the player from across the room is worth telling apart before
  -- walking into the open, and a colour alone would not say so.
  | Set.member pos (viewShooters view) =
      withAttr (attrName "shooter") $ str "A"
  | Set.member pos (viewMonsters view) =
      withAttr (attrName "monster") $ str "M"
  | Set.member pos (viewItems view) =
      withAttr (attrName "item") $ str "!"
  | Set.member pos (viewNpcs view) =
      withAttr (attrName "npc") $ str "N"
  -- A corpse lies on the floor, not over a staircase. Something died on the
  -- stairs down on floor 7 and the marker sat on top of them for the rest
  -- of the run: you could stand on the way down and be told nothing.
  -- Told apart from a monster's corpse on purpose: one is something the
  -- player killed, the other is the player.
  | Set.member pos (viewGraves view) && tile `elem` [Floor, Start] =
      withAttr (attrName "grave") $ str "\8225"
  | Set.member pos (viewCorpses view) && tile `elem` [Floor, Start] =
      withAttr (attrName "corpse") $ str "†"
  -- Left on the floor once the player moves off it, so the way they came is
  -- marked with what it cost them. Not "^": that is a shaft, and a way up
  -- is not a thing to confuse with a blade.
  -- A doorway you can walk through against one you cannot: the difference
  -- decides whether a corridor is a way out or a wall, and it changes as
  -- the player opens and shuts them.
  | Set.member pos (viewOpenDoors view), tile == Door =
      withAttr (attrName "door") $ str "'"
  | Set.member pos (viewSprung view) && tile `elem` [Floor, Start] =
      withAttr (attrName "sprung") $ str "*"
  | otherwise =
      drawTile tile

-- Helper to render a hidden tile (e.g., in fog or discovered but not visible)
-- Remembered terrain. What is drawn is what a player would still know
-- about a tile they have been to and cannot currently see: the shape of the
-- walls, and the way out.
--
-- Stairs and doors used to be drawn as plain floor once they were out of
-- sight, so a floor mapped by a Miner's Lantern showed everything except
-- the one thing a map is for.
drawTileHidden :: Tile -> Widget ()
drawTileHidden Wall      = str "#"
drawTileHidden Floor     = str "."
drawTileHidden Door      = str "+"
drawTileHidden UpStair   = str "<"
drawTileHidden DownStair = str ">"
drawTileHidden Start     = str "."
drawTileHidden Shaft     = str "^"

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
  [ "Reached floor " ++ show (deepestLevel state + 1) ++ " of " ++ show (length (levels state))
  , "Treasure " ++ (if gameWon state then "carried out" else "lost") ++ ": "
      ++ show (treasureCarried state)
  , "Experience: " ++ show (xp (player state))
  ]

-- One row of the scoreboard, in fixed columns so the numbers line up.
--
-- The dungeon never changes, which is the point of keeping these: every row
-- is the same twelve floors, so the rows can be read against each other.
scoreRow :: Int -> Run -> String
scoreRow place run =
  pad 4 (show place ++ ".")
    ++ pad 18 (runWhen run)
    ++ pad 8 (case runEnding run of GotOut -> "out"; Killed -> "died")
    ++ pad 8 ("F" ++ show (runDepth run))
    ++ rpad 9 (show (runTreasure run))
    ++ rpad 8 (show (runXP run))
    ++ rpad 8 (show (runTurns run))
    ++ rpad 9 (show (runScore run))
  where
    pad n t = t ++ replicate (n - length t) ' '
    rpad n t = replicate (n - length t) ' ' ++ t

scoreHeader :: String
scoreHeader =
  "    " ++ "when              " ++ "how     " ++ "depth   "
    ++ " treasure" ++ "      xp" ++ "   turns" ++ "    score"

-- | The scoreboard with one run picked out, as rows of text. The run being
-- shown may not be on the board yet -- it is written when the game exits,
-- and the player wants to see where they came before that.
scoreLines :: Maybe Run -> [Run] -> [String]
scoreLines highlight runs
  | null table = ["Nothing recorded yet. This is the first."]
  | otherwise = scoreHeader : zipWith row [1 ..] table
  where
    table = take 10 (ranked (maybe runs (: runs) highlight))
    row place run
      | Just run == highlight = "> " ++ drop 2 (scoreRow place run)
      | otherwise = scoreRow place run

-- | The messages, as far back as they are kept.
--
-- The pane on the main screen shows five, which is enough to follow a fight
-- and not enough to look anything up: a trigger's message, or what an NPC
-- said, is gone by the time the player wonders about it.
--
-- Oldest at the top and newest at the bottom, the way the pane reads, so
-- the last line of this is the last line of that.
drawLogPopup :: GameState -> Widget ()
drawLogPopup state =
  C.centerLayer $
    B.borderWithLabel (str label) $
      padAll 1 $ hLimit width $ padRight Max $ vBox (map str body)
  where
    kept = message state
    -- The list runs newest first, so the offset counts from its head and
    -- the window is turned round to read oldest at the top.
    shown = reverse (take loggedOnScreen (drop (logScroll state) kept))
    -- Which lines these are, numbered from the oldest, so that scrolling
    -- shows where in the history it has got to rather than only how much of
    -- it is on screen.
    newest = length kept - logScroll state
    oldest = newest - length shown + 1
    label
      | null kept = "Messages"
      | otherwise = "Messages (" ++ show oldest ++ "-" ++ show newest
                      ++ " of " ++ show (length kept) ++ ")"
    footer
      | length kept <= loggedOnScreen = "Press any key to close."
      | otherwise = "j/k to scroll, g/G for the ends, any other key closes."
    body
      | null kept = ["Nothing has happened yet."]
      | otherwise = shown ++ [" ", footer]
    width = maximum (length label : map length body)

drawScoresPopup :: GameState -> Widget ()
drawScoresPopup state =
  C.centerLayer $
    B.borderWithLabel (str "Scores") $
      padAll 1 $ vBox $
        map str (scoreLines Nothing (scoreboard state))
          ++ [str " ", C.hCenter (str "Press any key to close.")]

-- What the run just finished was worth, and how it sits among the rest.
endOfRunBoard :: GameState -> [String]
endOfRunBoard state = case runOf "this run" state of
  Nothing -> []
  Just run -> " " : scoreLines (Just run) (scoreboard state)

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
          ++ map (C.hCenter . str) (endOfRunBoard state)
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
          ++ map (C.hCenter . str) (endOfRunBoard state)
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
    keyed = keyedInventory inv eqpdWeapon eqpdArmor
    labels = map (inventoryEntry eqpdWeapon eqpdArmor) keyed
    -- The names are padded to a common width so the second column lines up,
    -- which is what makes the list readable as a table rather than prose.
    -- The padding is capped, because one very long name would otherwise
    -- push what every item does off the right of an 80-column terminal --
    -- and the second column is the reason the chooser has two.
    nameWidth = min nameColumn (maximum (0 : map length labels))
    entries
      | null inv = ["No items collected"]
      | otherwise =
          [ padTo nameWidth label ++ "   " ++ whatItDoes itm
          | (label, (_, itm)) <- zip labels keyed
          ]
    width = maximum (length title : map length entries)

-- | A field of exactly n characters: padded if it is short, and cut with a
-- tilde if it is long, so that a name nobody expected cannot move the
-- column after it.
padTo :: Int -> String -> String
padTo n text
  | length text <= n = text ++ replicate (n - length text) ' '
  | otherwise = take (n - 1) text ++ "~"

-- | How much of the chooser the names may take. What is left over is enough
-- for the longest thing whatItDoes says, inside 80 columns.
nameColumn :: Int
nameColumn = 30

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
drawStatsBox :: GameState -> Widget ()
drawStatsBox state =
    hLimit 30 $
      B.borderWithLabel (str "Stats") $
        vBox
          [ padRight Max $ str $ "Level: " ++ show (playerXPLevel plyr)
          , padRight Max $ str $ "HP: " ++ show (health plyr)
          , padRight Max $ str $ "Attack: " ++ show (attack plyr) ++ " (Base: " ++ show (baseAttack plyr) ++ ")"
          , padRight Max $ str $ "Resistance: " ++ show (resistance plyr) ++ " (Base: " ++ show (baseResistance plyr) ++ ")"
          , padRight Max $ str $ "XP: " ++ show (xp plyr) ++ toNextLevel
          , padRight Max $ str $ "Treasure: " ++ show (sum (map iValue (inventory plyr)))
          ]
  where
    plyr = player state
    -- How much more is wanted for the next rung, on the same line as the
    -- experience itself: the box is thirty columns wide and a player wants
    -- to read the two figures against each other anyway.
    toNextLevel = case nextXPLevel state of
      Nothing -> " (top level)"
      Just (_, wanted) -> " (" ++ show wanted ++ " to next)"

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
