-- test/Fixtures.hs
--
-- Small hand-built worlds and entities for the specs. Everything here is
-- deliberately tiny so that expected values can be worked out by hand.
module Fixtures where

import Game.State (Problems, charToTile, initializeGrid)
import Game.Types
import Data.List (isInfixOf)
import Linear.V2 (V2 (..))
import System.Random (StdGen, mkStdGen)
import Test.Hspec
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
    , sprung             = []
    , graves             = []
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

-- | A fixed generator, so that a test rolls the same numbers every run.
testGen :: StdGen
testGen = mkStdGen 2026

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
    , legendPage        = 0
    , keyPressCount     = 0
    , lastInteractedNpc = Nothing
    , aimingState       = Nothing
    , gameOver          = False
    , gameWon           = False
    , rng               = testGen
    , hiddenTurns       = 0
    , defeatedMonsters  = []
    , deepestLevel      = 0
    , turnCount         = 0
    , scoreboard        = []
    , showScores        = False
    , showLog           = False
    , logScroll         = 0
    , boons             = []
    , boonChoice        = Nothing
    , keysPressed       = ""
    }

-- | The standard fixture: the open room with the player on the start tile.
baseState :: GameState
baseState = mkState (mkWorld openMap) (V2 4 3)

-- | A treasure worth carrying out.
mkTreasure :: String -> Int -> Item
mkTreasure n worth = (mkItem n Special 0 (V2 0 0)) {iEffect = Just Keepsake, iValue = worth}

-- | A Special item with the given effect.
mkSpecial :: String -> ItemEffect -> Int -> Item
mkSpecial n effect val =
  (mkItem n Special val (V2 0 0)) {iEffect = Just effect}

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
    , iEffect      = Nothing
    , iValue       = 0
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
    , mRange      = Nothing
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
  DoorEntity {dePosition = pos, deLocked = locked, deShut = locked, deKeyName = keyName}

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
    , FT.triggerMonsterName = Nothing
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

-- | The value a configuration transform produced, or a test failure naming
-- the problems it reported instead.
shouldSucceed :: Either Problems a -> IO a
shouldSucceed (Right a) = pure a
shouldSucceed (Left problems) = do
  expectationFailure ("expected success, but got: " ++ unlines problems)
  error "unreachable"

-- | Assert that a transform reported a problem mentioning some text.
shouldReport :: Either Problems a -> String -> Expectation
shouldReport (Left problems) needle
  | any (needle `isInfixOf`) problems = pure ()
  | otherwise = expectationFailure
      ("expected a problem mentioning " ++ show needle ++ ", but got:\n" ++ unlines problems)
shouldReport (Right _) needle =
  expectationFailure
    ("expected a problem mentioning " ++ show needle ++ ", but it succeeded")

-- | A level with nothing in it but a map.
jsonLevel :: [String] -> FT.MapLevel
jsonLevel grid =
  FT.MapLevel
    { FT.levelNumber = 1
    , FT.mapGrid = grid
    , FT.monsters = []
    , FT.doors = []
    , FT.items = []
    , FT.triggers = []
    , FT.npcs = []
    }

-- | A configuration with one XP level and the given map levels.
jsonConfig :: [FT.MapLevel] -> FT.GameConfig
jsonConfig lvls =
  FT.GameConfig
    { FT.xpLevels =
        [ FT.XPLevel
            { FT.xpLevel = 1, FT.xpThreshold = 0, FT.xpHealth = 20
            , FT.xpAttack = 5, FT.xpResistance = 1
            }
        ]
    , FT.levels = lvls
    }

-- | A monster standing at a position.
jsonMonsterAt :: String -> (Int, Int) -> FT.JSONMonster
jsonMonsterAt n pos =
  FT.JSONMonster
    {FT.name = n, FT.position = pos, FT.attack = 2, FT.health = 5, FT.xp = 1, FT.inactive = Just False, FT.range = Nothing}

-- | An NPC standing at a position.
jsonNpcAt :: String -> (Int, Int) -> FT.JSONNPC
jsonNpcAt n pos = FT.JSONNPC {FT.npcName = n, FT.npcPosition = pos, FT.npcMessage = "hello"}

-- | A door at a position.
jsonDoorAt :: (Int, Int) -> Bool -> String -> FT.JSONDoorEntity
jsonDoorAt pos locked keyName =
  FT.JSONDoorEntity {FT.doorPosition = pos, FT.doorLocked = locked, FT.doorKeyName = keyName}

-- | A floor item with the given name and category string.
jsonItemOf :: String -> String -> FT.JSONItem
jsonItemOf n cat =
  FT.JSONItem
    { FT.itemName = n
    , FT.itemPosition = (1, 1)
    , FT.itemDescription = ""
    , FT.itemCategory = cat
    , FT.itemEffectValue = 0
    , FT.itemHidden = False
    , FT.itemInactive = False
    , FT.itemUses = Nothing
    , FT.itemEffect = Nothing
    , FT.itemValue = Nothing
    }
