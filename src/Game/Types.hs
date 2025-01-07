-- src/Game/Types.hs
module Game.Types where

import Linear.V2 (V2)

-- Tile types for the map
data Tile = Wall | Floor | Door | UpStair | DownStair deriving (Eq, Show)

-- Directions for player movement
data Direction = North | South | East | West | Up | Down deriving (Eq, Show)

-- Category of Item
data ItemCategory = Armor | Weapon | Healing | Special deriving (Eq, Show)

-- Player data
data Player = Player
  { position   :: V2 Int
  , health     :: Int
  , attack     :: Int
  , resistance :: Int
  , inventory  :: [Item]
  , equippedWeapon :: Maybe Item
  , equippedArmor  :: Maybe Item
  } deriving (Show)

-- Enemy data
data Monster = Monster
  { mPosition :: V2 Int
  , mHealth   :: Int
  , mName     :: String
  } deriving (Show, Eq)

-- Item data
data Item = Item
  { iName        :: String
  , iDescription :: String
  , iPosition    :: V2 Int
  , iCategory    :: ItemCategory
  , iEffectValue :: Int
  } deriving (Show, Eq)

-- World state
data World = World
  { mapGrid    :: [[Tile]]
  , monsters   :: [Monster]
  , items      :: [Item]
  , visibility :: [[Bool]]
  , discovered :: [[Bool]]
  } deriving (Show)

-- Game state
data GameState = GameState
  { player        :: Player
  , levels        :: [World]  -- List of all levels
  , currentLevel  :: Int      -- Index of the current level
  , message       :: [String]
  , commandBuffer :: String
  , commandMode   :: Bool
  , gameOver      :: Bool
  } deriving (Show)
