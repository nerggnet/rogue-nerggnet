-- test/Fixtures.hs
--
-- Small hand-built worlds and entities for the specs. Everything here is
-- deliberately tiny so that expected values can be worked out by hand.
module Fixtures where

import Game.State (charToTile, initializeGrid)
import Game.Types
import Linear.V2 (V2 (..))
import qualified File.Types as FT

-- | A 9x7 room with no interior walls. @S@ (the start tile) is at @(4, 3)@.
--
-- @
--     012345678
--   0 #########
--   1 #.......#
--   2 #.......#
--   3 #...S...#
--   4 #.......#
--   5 #.......#
--   6 #########
-- @
openMap :: [String]
openMap =
  [ "#########"
  , "#.......#"
  , "#.......#"
  , "#...S...#"
  , "#.......#"
  , "#.......#"
  , "#########"
  ]

-- | The same room split by a wall at @x == 4@, for line-of-sight tests.
-- @S@ is at @(1, 3)@.
wallMap :: [String]
wallMap =
  [ "#########"
  , "#...#...#"
  , "#...#...#"
  , "#S..#...#"
  , "#...#...#"
  , "#...#...#"
  , "#########"
  ]

-- | Build a 'World' from an ASCII map using the same tile characters as
-- @world.json@.
mkWorld :: [String] -> World
mkWorld rows =
  World
    { mapGrid            = map (map charToTile) rows
    , mapRows            = nRows
    , mapCols            = nCols
    , monsters           = []
    , npcs               = []
    , items              = []
    , doors              = []
    , triggers           = []
    , visibility         = initializeGrid False nRows nCols
    , discovered         = initializeGrid False nRows nCols
    , discoveredCoords   = []
    , tileOverrides      = []
    , corpses            = []
    }
  where
    nRows = length rows
    nCols = case rows of
      []      -> 0
      (r : _) -> length r

-- | Three XP levels with round numbers: level 1 starts at 20 HP / 5 attack /
-- 1 resistance, and the thresholds are 0, 100 and 250.
testXPLevels :: [XPLevel]
testXPLevels =
  [ XPLevel {xpLevel = 1, xpThreshold = 0,   xpHealth = 20, xpAttack = 5,  xpResistance = 1}
  , XPLevel {xpLevel = 2, xpThreshold = 100, xpHealth = 40, xpAttack = 8,  xpResistance = 2}
  , XPLevel {xpLevel = 3, xpThreshold = 250, xpHealth = 60, xpAttack = 11, xpResistance = 3}
  ]

mkPlayer :: V2 Int -> Player
mkPlayer pos =
  Player
    { position       = pos
    , health         = 20
    , baseAttack     = 5
    , baseResistance = 1
    , attack         = 5
    , resistance     = 1
    , xp             = 0
    , playerXPLevel  = 1
    , inventory      = []
    , equippedWeapon = Nothing
    , equippedArmor  = Nothing
    }

-- | A single-level game state with the player at the given position.
mkState :: World -> V2 Int -> GameState
mkState world pos =
  GameState
    { player            = mkPlayer pos
    , xpLevels          = testXPLevels
    , levels            = [world]
    , currentLevel      = 0
    , message           = []
    , commandBuffer     = ""
    , commandMode       = False
    , commandToExecute  = False
    , inventoryMode     = Nothing
    , showLegend        = False
    , keyPressCount     = 0
    , lastInteractedNpc = Nothing
    , aimingState       = Nothing
    , gameOver          = False
    , gameWon           = False
    }

-- | The standard fixture: the open room with the player on the start tile.
baseState :: GameState
baseState = mkState (mkWorld openMap) (V2 4 3)

mkItem :: String -> ItemCategory -> Int -> V2 Int -> Item
mkItem n cat val pos =
  Item
    { iName        = n
    , iDescription = "a test item"
    , iPosition    = pos
    , iCategory    = cat
    , iEffectValue = val
    , iHidden      = False
    , iInactive    = False
    , iUses        = Nothing
    }

mkMonster :: String -> V2 Int -> Int -> Int -> Monster
mkMonster n pos hp atk =
  Monster
    { mPosition   = pos
    , mHealth     = hp
    , mAttack     = atk
    , mName       = n
    , mXP         = 10
    , mInactive   = False
    , mAttackWait = True
    }

mkNPC :: String -> V2 Int -> NPC
mkNPC n pos =
  NPC
    { npcName               = n
    , npcPosition           = pos
    , npcMessage            = "hello"
    , npcPreferredDirection = Nothing
    }

mkDoor :: V2 Int -> Bool -> String -> DoorEntity
mkDoor pos locked keyName =
  DoorEntity {dePosition = pos, deLocked = locked, deKeyName = keyName}

mkTrigger :: TriggerCondition -> [Action] -> Bool -> Trigger
mkTrigger cond as recurring =
  Trigger
    { triggerCondition = cond
    , triggerActions   = as
    , triggerRecurring = recurring
    }

-- | A 'FT.JSONTrigger' with every optional field cleared, to be filled in with
-- record update syntax.
baseJSONTrigger :: FT.JSONTrigger
baseJSONTrigger =
  FT.JSONTrigger
    { FT.triggerType     = "position"
    , FT.target          = Nothing
    , FT.requiredItems   = Nothing
    , FT.triggerItemName = Nothing
    , FT.triggerNpcName  = Nothing
    , FT.actions         = []
    , FT.message         = ""
    , FT.recurring       = False
    }

-- | Modify the current level of a single-level state.
withWorld :: (World -> World) -> GameState -> GameState
withWorld f state = state {levels = map f (levels state)}

withPlayer :: (Player -> Player) -> GameState -> GameState
withPlayer f state = state {player = f (player state)}

-- | Look up a tile by @(x, y)@.
tileAt :: V2 Int -> World -> Tile
tileAt (V2 x y) world = mapGrid world !! y !! x

visibleAt :: V2 Int -> World -> Bool
visibleAt (V2 x y) world = visibility world !! y !! x

discoveredAt :: V2 Int -> World -> Bool
discoveredAt (V2 x y) world = discovered world !! y !! x
