-- src/Game/Types.hs
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson.Key as Key
import Linear.V2 (V2)
import System.Random (StdGen, mkStdGen)
import System.Random.Internal (StdGen (..))
import System.Random.SplitMix (seedSMGen, unseedSMGen)
import qualified Data.Set as Set

-- Custom JSON instances for V2
instance ToJSON a => ToJSON (V2 a)
instance FromJSON a => FromJSON (V2 a)

-- StdGen carries no JSON instances of its own. Its two words are enough to
-- rebuild the identical stream, so a reloaded game carries on rolling where
-- it left off rather than starting the sequence again.
instance ToJSON StdGen where
  toJSON gen = toJSON (unseedSMGen (unStdGen gen))

instance FromJSON StdGen where
  parseJSON value = do
    (seed, gamma) <- parseJSON value
    pure (StdGen (seedSMGen seed gamma))

-- | Shaft is a crack in the ceiling with daylight behind it. It is walked
-- over like floor; a rope turns it into a way up.
data Tile = Wall | Floor | Door | UpStair | DownStair | Start | Shaft
  deriving (Eq, Show, Generic, Enum, Bounded)

instance ToJSON Tile
instance FromJSON Tile

-- | How a run finished. There is no third case: a run still being played
-- is not a run yet.
data Ending = GotOut | Killed deriving (Eq, Show, Generic)

instance ToJSON Ending
instance FromJSON Ending

-- | A finished run, as the scoreboard keeps it. The dungeon is drawn by
-- hand and never changes, so two people's runs are of the same twelve
-- floors and can be set side by side -- which only works if they are
-- written down.
data Run = Run
  { runWhen     :: String -- ^ When it ended, as the caller chose to write it
  , runEnding   :: Ending
  , runDepth    :: Int    -- ^ Deepest floor reached, counting from 1
  , runTreasure :: Int    -- ^ What came out. Dying brings nothing out
  , runXP       :: Int
  , runTurns    :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON Run
instance FromJSON Run

-- | Where a previous run ended badly, and what it was carrying when it did.
--
-- The dungeon never changes, so the floor a run died on is the same floor
-- the next run will walk through: the body can be put back exactly where it
-- fell, with everything it was carrying still on it.
data Grave = Grave
  { graveWhen     :: String  -- ^ When that run ended
  , graveWorld    :: String  -- ^ The dungeon it died in, so another one's dead stay out
  , graveFloor    :: Int     -- ^ Counting from zero, as levels are indexed
  , graveAt       :: V2 Int
  , graveCarried  :: [Item]
  , graveTreasure :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON Grave
instance FromJSON Grave

data Direction = North | South | East | West | Up | Down deriving (Eq, Show, Generic)

instance ToJSON Direction
instance FromJSON Direction

data ItemCategory = Armor | Weapon | Range | Healing | Special | Key deriving (Eq, Show, Generic)

instance ToJSON ItemCategory
instance FromJSON ItemCategory

-- | What a Special item does.
--
-- Everything else in ItemCategory has behaviour baked into the game: armour
-- is worn, keys unlock, potions heal. A Special item says here what it is
-- for, so new ones can be written in world.json rather than in Haskell.
data ItemEffect
  = Keepsake   -- ^ Nothing; carried for a trigger, or for its own sake
  | Empower    -- ^ Raises attack for good
  | Fortify    -- ^ Raises resistance for good
  | Reveal     -- ^ Maps the whole level
  | Blink      -- ^ Moves the player elsewhere on the level
  | Firestorm  -- ^ Hurts every monster in sight
  | Regenerate -- ^ While carried, heals a little each turn
  | Lifesteal  -- ^ While carried, returns a share of the damage dealt
  | Revive     -- ^ While carried, saves the player from one death
  | Vanish     -- ^ Hides the player from monsters for a while
  | Escape     -- ^ On a shaft, climbs to the floor above; from the top, out
  deriving (Eq, Show, Generic, Enum, Bounded)

instance ToJSON ItemEffect
instance FromJSON ItemEffect

-- | Effects that are spent by using them, rather than working while carried.
-- Escape is not here, and nor is Revive: both are spent only when they
-- actually do something. A rope used where there is no shaft is a wasted
-- keypress, not a lost rope.
spentOnUse :: ItemEffect -> Bool
spentOnUse effect = effect `elem` [Empower, Fortify, Reveal, Blink, Firestorm, Vanish]

data InventoryMode = UseMode | DropMode deriving (Eq, Show, Generic)

instance ToJSON InventoryMode
instance FromJSON InventoryMode

data Player = Player
  { position       :: V2 Int
  , health         :: Int
  , baseAttack     :: Int
  , baseResistance :: Int
  , attack         :: Int
  , resistance     :: Int
  , xp             :: Int
  , playerXPLevel  :: Int
  , inventory      :: [Item]
  , equippedWeapon :: Maybe Item
  , equippedArmor  :: Maybe Item
  } deriving (Show, Generic)

instance ToJSON Player
instance FromJSON Player

data XPLevel = XPLevel
  { xpLevel       :: Int
  , xpThreshold :: Int
  , xpHealth      :: Int
  , xpAttack      :: Int
  , xpResistance  :: Int
  } deriving (Generic)

instance ToJSON XPLevel
instance FromJSON XPLevel

data Monster = Monster
  { mPosition   :: V2 Int
  , mHealth     :: Int
  , mAttack     :: Int
  , mName       :: String
  , mXP         :: Int
  , mInactive   :: Bool
  , mAttackWait :: Bool
  , mRange      :: Maybe Int -- ^ How far it can strike. Nothing is arm's length
  } deriving (Show, Eq, Generic)

instance ToJSON Monster

-- Hand-written and tolerant, so that a save written before monsters could
-- shoot still loads: anything without a range fights at arm's length, which
-- is what every monster used to do.
instance FromJSON Monster where
  parseJSON = withObject "Monster" $ \v ->
    Monster
      <$> v .:  Key.fromString "mPosition"
      <*> v .:  Key.fromString "mHealth"
      <*> v .:  Key.fromString "mAttack"
      <*> v .:  Key.fromString "mName"
      <*> v .:  Key.fromString "mXP"
      <*> v .:? Key.fromString "mInactive" .!= False
      <*> v .:? Key.fromString "mAttackWait" .!= False
      <*> v .:? Key.fromString "mRange" .!= Nothing

data NPC = NPC
  { npcName               :: String
  , npcPosition           :: V2 Int
  , npcMessage            :: String
  , npcPreferredDirection :: Maybe Direction
  } deriving (Show, Eq, Generic)

instance ToJSON NPC
instance FromJSON NPC

data Item = Item
  { iName        :: String
  , iDescription :: String
  , iPosition    :: V2 Int
  , iCategory    :: ItemCategory
  , iEffectValue :: Int
  , iHidden      :: Bool
  , iInactive    :: Bool
  , iUses        :: Maybe Int
  , iEffect      :: Maybe ItemEffect -- What a Special item does
  , iValue       :: Int              -- What it is worth carried out of the dungeon
  } deriving (Show, Eq, Generic)

instance ToJSON Item

-- Read field by field, so that an item saved before a field existed still
-- loads with a sensible value for it rather than failing the whole save.
instance FromJSON Item where
  parseJSON = withObject "Item" $ \v ->
    Item
      <$> v .:  Key.fromString "iName"
      <*> v .:? Key.fromString "iDescription" .!= ""
      <*> v .:  Key.fromString "iPosition"
      <*> v .:  Key.fromString "iCategory"
      <*> v .:? Key.fromString "iEffectValue" .!= 0
      <*> v .:? Key.fromString "iHidden" .!= False
      <*> v .:? Key.fromString "iInactive" .!= False
      <*> v .:? Key.fromString "iUses" .!= Nothing
      <*> v .:? Key.fromString "iEffect" .!= Nothing
      <*> v .:? Key.fromString "iValue" .!= 0

-- | A door. Shut and locked are different things: a locked door wants its
-- key, a shut one only wants pushing. Both stop what is on the other side,
-- which is the whole use of being able to shut one.
data DoorEntity = DoorEntity
  { dePosition :: V2 Int
  , deLocked   :: Bool
  , deShut     :: Bool
  , deKeyName  :: String
  } deriving (Show, Eq, Generic)

-- | Whether a door stands in the way -- of walking through it, and of
-- seeing past it.
deBlocks :: DoorEntity -> Bool
deBlocks d = deLocked d || deShut d

instance ToJSON DoorEntity

instance FromJSON DoorEntity where
  parseJSON = withObject "DoorEntity" $ \v ->
    DoorEntity
      <$> v .:  Key.fromString "dePosition"
      <*> v .:  Key.fromString "deLocked"
      <*> v .:? Key.fromString "deShut" .!= False
      <*> v .:  Key.fromString "deKeyName"

-- | What makes a trigger fire.
--
-- This is data rather than a @GameState -> Bool@ so that triggers can be
-- saved and loaded directly. 'Game.State.evalTriggerCondition' interprets it.
data TriggerCondition
  = AtPosition (V2 Int)                   -- ^ The player is standing here
  | AtPositionWithItems (V2 Int) [String] -- ^ ...and is carrying all of these
  | HasItem String                        -- ^ The item is in the inventory
  | TalkedToNpc String                    -- ^ The player just talked to this NPC
  | MonsterDefeated String                -- ^ A monster of this name has been beaten
  | AllMonstersDefeated                   -- ^ No active monsters are left
  deriving (Show, Eq, Generic)

instance ToJSON TriggerCondition
instance FromJSON TriggerCondition

data Trigger = Trigger
  { triggerCondition :: TriggerCondition -- Condition for activation
  , triggerActions   :: [Action]         -- Actions to execute
  , triggerRecurring :: Bool             -- Will this trigger fire once or be recurring
  } deriving (Show, Eq, Generic)

instance ToJSON Trigger
instance FromJSON Trigger

data Action
  = SpawnItem String (V2 Int)        -- Item name and position
  | SpawnMonster String (V2 Int)     -- Monster name and position
  | UnlockDoor (V2 Int)              -- Position of the door
  | ShiftTile (V2 Int) Tile          -- Position and new tile type
  | TransportPlayer (V2 Int)         -- Target position for the player
  | ConsumeItem String               -- Remove item from inventory
  | AddToInventory String            -- Add an item to the player's inventory
  | HarmPlayer Int                   -- A trap springs, for this much damage
  | HealPlayer Int                   -- A fountain, a shrine, a blessing
  | DisplayMessage String            -- Message to display
  | SetGameWon                       -- Indicate that the game has been won
  deriving (Show, Eq, Generic)

instance ToJSON Action
instance FromJSON Action

data World = World
  { mapGrid    :: [[Tile]]
  , mapRows    :: Int
  , mapCols    :: Int
  , monsters   :: [Monster]
  , npcs       :: [NPC]
  , items      :: [Item]
  , doors      :: [DoorEntity]
  , triggers   :: [Trigger]
  , visibility :: [[Bool]]
  , discovered :: [[Bool]]
  , discoveredCoords   :: [(Int, Int)]
  , tileOverrides      :: [(V2 Int, Tile)]
  , corpses            :: [V2 Int]  -- Where monsters have been defeated
  , sprung             :: [V2 Int]  -- Where something in the floor has gone off
  , graves             :: [Grave]   -- Where earlier runs of this dungeon ended
  } deriving (Generic)

instance ToJSON World where
  toJSON world =
    object
      [ Key.fromString "mapGrid" .= mapGrid world
      , Key.fromString "mapRows" .= mapRows world
      , Key.fromString "mapCols" .= mapCols world
      , Key.fromString "monsters" .= monsters world
      , Key.fromString "npcs" .= npcs world
      , Key.fromString "items" .= items world
      , Key.fromString "doors" .= doors world
      , Key.fromString "triggers" .= triggers world
      , Key.fromString "visibility" .= visibility world
      , Key.fromString "discoveredCoords" .= discoveredCoords world
      , Key.fromString "tileOverrides" .= tileOverrides world
      , Key.fromString "corpses" .= corpses world
      ]

-- Convert the discovered grid to a list of coordinates
gridToCoords :: [[Bool]] -> [(Int, Int)]
gridToCoords grid = [ (x, y) | (y, row) <- zip [0..] grid , (x, cell) <- zip [0..] row , cell ]

instance FromJSON World where
  parseJSON = withObject "World" $ \v -> do
    trggrs <- v .: Key.fromString "triggers"
    grid <- v .: Key.fromString "mapGrid"
    gridRows <- v .: Key.fromString "mapRows"
    gridCols <- v .: Key.fromString "mapCols"
    dscvrdCoords <- v .: Key.fromString "discoveredCoords" -- Parse the coordinates directly
    let dscvrd = if gridRows > 0 && gridCols > 0 then coordsToGrid dscvrdCoords gridRows gridCols else []
    mnstrs <- v .: Key.fromString "monsters"
    ns <- v .: Key.fromString "npcs"
    itms <- v .: Key.fromString "items"
    drs <- v .: Key.fromString "doors"
    vsblt <- v .: Key.fromString "visibility"
    tileOvrrds <- v .: Key.fromString "tileOverrides"
    crpses <- v .:? Key.fromString "corpses" .!= []
    sprng <- v .:? Key.fromString "sprung" .!= []
    grvs <- v .:? Key.fromString "graves" .!= []
    return World
      { mapGrid = grid
      , mapRows = gridRows
      , mapCols = gridCols
      , monsters = mnstrs
      , npcs = ns
      , items = itms
      , doors = drs
      , triggers = trggrs
      , visibility = vsblt
      , discovered = dscvrd
      , discoveredCoords = dscvrdCoords
      , tileOverrides = tileOvrrds
      , corpses = crpses
      , sprung = sprng
      , graves = grvs
      }

-- Convert a list of discovered coordinates back to a 2D grid.
-- The coordinates go into a Set first: a level with a few hundred discovered
-- tiles would otherwise scan the whole list once per cell of the grid.
coordsToGrid :: [(Int, Int)] -> Int -> Int -> [[Bool]]
coordsToGrid coords rows cols =
  [ [ (x, y) `Set.member` seen | x <- [0 .. cols - 1] ] | y <- [0 .. rows - 1] ]
  where
    seen = Set.fromList coords

newtype AimingState = AimingState
  { aimingItem :: Item -- The ranged item being used
  } deriving (Generic, Eq)

instance ToJSON AimingState
instance FromJSON AimingState

data GameState = GameState
  { player            :: Player
  , xpLevels          :: [XPLevel]
  , levels            :: [World]
  , currentLevel      :: Int
  , message           :: [String]
  , commandBuffer     :: String
  , commandMode       :: Bool
  , commandToExecute  :: Bool
  , inventoryMode     :: Maybe InventoryMode
  , legendPage        :: Int -- 0 when the help is closed
  , keyPressCount     :: Int
  , lastInteractedNpc :: Maybe String
  , aimingState       :: Maybe AimingState
  , gameOver          :: Bool
  , gameWon           :: Bool
  , rng               :: StdGen -- Every roll the game makes comes from here
  , hiddenTurns       :: Int -- Turns left before monsters notice the player again
  , defeatedMonsters  :: [String] -- Names of monsters beaten so far
  , deepestLevel      :: Int -- The furthest down the player has been
  , turnCount         :: Int -- Turns this run has lasted, for the scoreboard
  , scoreboard        :: [Run] -- Finished runs, best first; read at startup
  , showScores        :: Bool  -- Whether the scoreboard popup is open
  , showLog           :: Bool  -- Whether the message history is open
  , keysPressed       :: String -- The run so far, newest first, for a replay
  } deriving (Generic)

instance ToJSON GameState

-- Only the durable parts of a game are required. Everything to do with what
-- is on screen right now defaults, so a save written by a version that did
-- not have a field, or that had a different one, still loads.
instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \v ->
    GameState
      <$> v .:  Key.fromString "player"
      <*> v .:  Key.fromString "xpLevels"
      <*> v .:  Key.fromString "levels"
      <*> v .:  Key.fromString "currentLevel"
      <*> v .:? Key.fromString "message" .!= []
      <*> v .:? Key.fromString "commandBuffer" .!= ""
      <*> v .:? Key.fromString "commandMode" .!= False
      <*> v .:? Key.fromString "commandToExecute" .!= False
      <*> v .:? Key.fromString "inventoryMode" .!= Nothing
      <*> v .:? Key.fromString "legendPage" .!= 0
      <*> v .:? Key.fromString "keyPressCount" .!= 0
      <*> v .:? Key.fromString "lastInteractedNpc" .!= Nothing
      <*> v .:? Key.fromString "aimingState" .!= Nothing
      <*> v .:? Key.fromString "gameOver" .!= False
      <*> v .:? Key.fromString "gameWon" .!= False
      -- A save from before the game rolled dice has no generator to restore.
      <*> v .:? Key.fromString "rng" .!= mkStdGen 0
      <*> v .:? Key.fromString "hiddenTurns" .!= 0
      <*> v .:? Key.fromString "defeatedMonsters" .!= []
      <*> v .:? Key.fromString "deepestLevel" .!= 0
      <*> v .:? Key.fromString "turnCount" .!= 0
      -- The scoreboard lives in its own file and is read at startup;
      -- a save carries neither it nor whether it was on screen.
      <*> pure []
      <*> pure False
      <*> pure False
      <*> pure ""
