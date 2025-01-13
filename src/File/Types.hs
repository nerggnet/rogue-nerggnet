-- src/File/Types.hs
{-# LANGUAGE DeriveGeneric #-}

module File.Types where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | JSON representation of a map level
data MapLevel = MapLevel
  { levelNumber :: Int              -- Level identifier
  , mapGrid     :: [String]         -- Serialized map grid (row-wise string representation)
  , monsters    :: [JSONMonster]    -- Monsters present on this level
  , doors       :: [JSONDoorEntity] -- Doors present on this level
  , items       :: [JSONItem]       -- Items present on this level
  , triggers    :: [JSONTrigger]   -- Triggers on this level
  , npcs        :: [JSONNPC]        -- NPCs on the map level
  } deriving (Show, Generic)

instance FromJSON MapLevel
instance ToJSON MapLevel

-- | JSON representation of a monster
data JSONMonster = JSONMonster
  { name     :: String      -- Monster name
  , position :: (Int, Int)  -- Monster's position (row, col)
  , attack   :: Int         -- Monster's attack
  , health   :: Int         -- Monster health
  , xp       :: Int         -- XP gained from defeating the monster
  } deriving (Show, Generic)

instance FromJSON JSONMonster
instance ToJSON JSONMonster

-- | JSON representation of an item
data JSONItem = JSONItem
  { itemName        :: String      -- Item name
  , itemPosition    :: (Int, Int)  -- Item's position (row, col)
  , itemDescription :: String      -- Item description
  , itemCategory    :: String      -- Item category, i.e. Armor, Weapon, Healing, Special, or Key
  , itemEffectValue :: Int         -- Item effect value, e.g. increases attack with this value
  , itemHidden      :: Bool        -- Controls whether or not an item is visible
  , itemInactive    :: Bool        -- Controls whether or not an item can be found and picked up
  } deriving (Show, Generic)

instance FromJSON JSONItem
instance ToJSON JSONItem

-- | JSON representation of a door
data JSONDoorEntity = JSONDoorEntity
  { doorPosition :: (Int, Int)
  , doorLocked   :: Bool
  } deriving (Show, Generic)

instance FromJSON JSONDoorEntity
instance ToJSON JSONDoorEntity

-- src/File/Types.hs
data JSONTrigger = JSONTrigger
  { triggerType     :: String              -- Type of trigger: "position", "itemPickup", etc.
  , target          :: Maybe (Int, Int)    -- Target position (for position triggers)
  , triggerItemName :: Maybe String        -- Item name (for itemPickup triggers)
  , triggerNpcName  :: Maybe String        -- NPC name (for npcTalked triggers)
  , actions         :: [JSONTriggerAction] -- List of actions to execute
  , message         :: String              -- Message to display when triggered
  } deriving (Show, Generic)

instance FromJSON JSONTrigger
instance ToJSON JSONTrigger

data JSONTriggerAction = JSONTriggerAction
  { actionType     :: String           -- Action type, e.g., "spawnItem", "unlockDoor"
  , actionPosition :: Maybe (Int, Int) -- General position for actions
  , actionItemName :: Maybe String     -- For "spawnItem", the item's name
  , actionTileType :: Maybe Char       -- For "shiftTile", the tile's new type
  , actionMessage  :: Maybe String     -- For "displayMessage", the custom message
  } deriving (Show, Generic)

instance FromJSON JSONTriggerAction
instance ToJSON JSONTriggerAction

-- | JSON representation of an NPC
data JSONNPC = JSONNPC
  { npcName     :: String        -- NPC name
  , npcPosition :: (Int, Int)    -- NPC's position (row, col)
  , npcMessage  :: String        -- Message the NPC displays
  } deriving (Show, Generic)

instance FromJSON JSONNPC
instance ToJSON JSONNPC
