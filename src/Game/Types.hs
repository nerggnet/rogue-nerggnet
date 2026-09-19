-- src/Game/Types.hs
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson.Key as Key
import Linear.V2 (V2)

-- Custom JSON instances for V2
instance ToJSON a => ToJSON (V2 a)
instance FromJSON a => FromJSON (V2 a)

data Tile = Wall | Floor | Door | UpStair | DownStair | Start deriving (Eq, Show, Generic)

instance ToJSON Tile
instance FromJSON Tile

data Direction = North | South | East | West | Up | Down deriving (Eq, Show, Generic)

instance ToJSON Direction
instance FromJSON Direction

data ItemCategory = Armor | Weapon | Range | Healing | Special | Key deriving (Eq, Show, Generic)

instance ToJSON ItemCategory
instance FromJSON ItemCategory

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
  } deriving (Show, Eq, Generic)

instance ToJSON Monster
instance FromJSON Monster

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
  } deriving (Show, Eq, Generic)

instance ToJSON Item
instance FromJSON Item

data DoorEntity = DoorEntity
  { dePosition :: V2 Int
  , deLocked   :: Bool
  , deKeyName  :: String
  } deriving (Show, Eq, Generic)

instance ToJSON DoorEntity
instance FromJSON DoorEntity

-- | What makes a trigger fire.
--
-- This is data rather than a @GameState -> Bool@ so that triggers can be
-- saved and loaded directly. 'Game.State.evalTriggerCondition' interprets it.
data TriggerCondition
  = AtPosition (V2 Int)                   -- ^ The player is standing here
  | AtPositionWithItems (V2 Int) [String] -- ^ ...and is carrying all of these
  | HasItem String                        -- ^ The item is in the inventory
  | TalkedToNpc String                    -- ^ The player just talked to this NPC
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
      }

-- Convert a list of discovered coordinates back to a 2D grid
coordsToGrid :: [(Int, Int)] -> Int -> Int -> [[Bool]]
coordsToGrid coords rows cols = [ [ (x, y) `elem` coords | x <- [0 .. cols - 1] ] | y <- [0 .. rows - 1] ]

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
  , showLegend        :: Bool
  , keyPressCount     :: Int
  , lastInteractedNpc :: Maybe String
  , aimingState       :: Maybe AimingState
  , gameOver          :: Bool
  , gameWon           :: Bool
  } deriving (Generic)

instance ToJSON GameState
instance FromJSON GameState
