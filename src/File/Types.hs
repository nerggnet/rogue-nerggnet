-- src/File/Types.hs
{-# LANGUAGE DeriveGeneric #-}

module File.Types where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | JSON representation of a map level
data MapLevel = MapLevel
  { levelNumber :: Int             -- Level identifier
  , mapGrid     :: [String]        -- Serialized map grid (row-wise string representation)
  , monsters    :: [JSONMonster]   -- Monsters present in this level
  , items       :: [JSONItem]      -- Items present in this level
  } deriving (Show, Generic)

instance FromJSON MapLevel
instance ToJSON MapLevel

-- | JSON representation of a monster
data JSONMonster = JSONMonster
  { name     :: String             -- Monster name
  , position :: (Int, Int)         -- Monster's position (row, col)
  , health   :: Int                -- Monster health
  } deriving (Show, Generic)

instance FromJSON JSONMonster
instance ToJSON JSONMonster

-- | JSON representation of an item
data JSONItem = JSONItem
  { itemName        :: String      -- Item name
  , itemPosition    :: (Int, Int)  -- Item's position (row, col)
  , itemDescription :: String      -- Item description
  , itemCategory    :: String      -- Item category, i.e. Armor, Weapon, Healing, or Special
  , itemEffectValue :: Int         -- Item effect value, e.g. increases attack with this value
  } deriving (Show, Generic)

instance FromJSON JSONItem
instance ToJSON JSONItem
