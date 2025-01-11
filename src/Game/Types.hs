-- src/Game/Types.hs
module Game.Types where

import Linear.V2 (V2)

data Tile = Wall | Floor | Door | UpStair | DownStair deriving (Eq, Show)

data Direction = North | South | East | West | Up | Down deriving (Eq, Show)

data ItemCategory = Armor | Weapon | Healing | Special | Key deriving (Eq, Show)

data Player = Player
  { position   :: V2 Int
  , health     :: Int
  , attack     :: Int
  , resistance :: Int
  , inventory  :: [Item]
  , equippedWeapon :: Maybe Item
  , equippedArmor  :: Maybe Item
  } deriving (Show)

data Monster = Monster
  { mPosition :: V2 Int
  , mHealth   :: Int
  , mAttack   :: Int
  , mName     :: String
  } deriving (Show, Eq)

data NPC = NPC
  { npcName    :: String
  , npcPosition :: V2 Int
  , npcMessage :: String
  , npcPreferredDirection :: Maybe Direction
  } deriving (Show, Eq)

data Item = Item
  { iName        :: String
  , iDescription :: String
  , iPosition    :: V2 Int
  , iCategory    :: ItemCategory
  , iEffectValue :: Int
  } deriving (Show, Eq)

data DoorEntity = DoorEntity
  { dePosition :: V2 Int
  , deLocked   :: Bool
  } deriving (Show, Eq)

data World = World
  { mapGrid    :: [[Tile]]
  , monsters   :: [Monster]
  , npcs       :: [NPC]
  , items      :: [Item]
  , doors      :: [DoorEntity]
  , visibility :: [[Bool]]
  , discovered :: [[Bool]]
  } deriving (Show)

data GameState = GameState
  { player        :: Player
  , levels        :: [World]
  , currentLevel  :: Int
  , message       :: [String]
  , commandBuffer :: String
  , commandMode   :: Bool
  , showLegend    :: Bool
  , keyPressCount :: Int
  , gameOver      :: Bool
  } deriving (Show)
