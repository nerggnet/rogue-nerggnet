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
  , npcs        :: [JSONNPC]        -- NPCs on the map level
  } deriving (Show, Generic)

instance FromJSON MapLevel
instance ToJSON MapLevel

-- | JSON representation of a monster
data JSONMonster = JSONMonster
  { name     :: String             -- Monster name
  , position :: (Int, Int)         -- Monster's position (row, col)
  , attack   :: Int                -- Monster's attack
  , health   :: Int                -- Monster health
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

-- | JSON representation of an NPC
data JSONNPC = JSONNPC
  { npcName     :: String        -- NPC name
  , npcPosition :: (Int, Int)    -- NPC's position (row, col)
  , npcMessage  :: String        -- Message the NPC displays
  } deriving (Show, Generic)

instance FromJSON JSONNPC
instance ToJSON JSONNPC
