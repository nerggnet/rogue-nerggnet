-- src/Game/Types.hs
module Game.Types where

import Linear.V2 (V2)

data Tile = Wall | Floor | Door | UpStair | DownStair deriving (Eq, Show)

data Direction = North | South | East | West | Up | Down deriving (Eq, Show)

data ItemCategory = Armor | Weapon | Healing | Special | Key deriving (Eq, Show)

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
  } deriving (Show)

data XPLevel = XPLevel
  { xpLevel       :: Int
  , xpThreshold :: Int
  , xpHealth      :: Int
  , xpAttack      :: Int
  , xpResistance  :: Int
  }

data Monster = Monster
  { mPosition :: V2 Int
  , mHealth   :: Int
  , mAttack   :: Int
  , mName     :: String
  , mXP       :: Int
  } deriving (Show, Eq)

data NPC = NPC
  { npcName               :: String
  , npcPosition           :: V2 Int
  , npcMessage            :: String
  , npcPreferredDirection :: Maybe Direction
  } deriving (Show, Eq)

data Item = Item
  { iName        :: String
  , iDescription :: String
  , iPosition    :: V2 Int
  , iCategory    :: ItemCategory
  , iEffectValue :: Int
  , iHidden      :: Bool
  , iInactive    :: Bool
  } deriving (Show, Eq)

data DoorEntity = DoorEntity
  { dePosition :: V2 Int
  , deLocked   :: Bool
  } deriving (Show, Eq)

data Trigger = Trigger
  { triggerCondition   :: GameState -> Bool -- Condition for activation
  , triggerActions     :: [Action]          -- Actions to execute
  , triggerDescription :: String            -- For debugging/logging
  }

data Action
  = SpawnItem String (V2 Int)        -- Item name and position
  | SpawnMonster String (V2 Int)     -- Monster name and position
  | UnlockDoor (V2 Int)              -- Position of the door
  | ShiftTile (V2 Int) Tile          -- Position and new tile type
  | TransportPlayer (V2 Int)         -- Target position for the player
  | DisplayMessage String            -- Message to display
  deriving (Show, Eq)

data World = World
  { mapGrid    :: [[Tile]]
  , monsters   :: [Monster]
  , npcs       :: [NPC]
  , items      :: [Item]
  , doors      :: [DoorEntity]
  , triggers   :: [Trigger]
  , visibility :: [[Bool]]
  , discovered :: [[Bool]]
  }

data GameState = GameState
  { player            :: Player
  , xpLevels          :: [XPLevel]
  , levels            :: [World]
  , currentLevel      :: Int
  , message           :: [String]
  , commandBuffer     :: String
  , commandMode       :: Bool
  , showLegend        :: Bool
  , keyPressCount     :: Int
  , lastInteractedNpc :: Maybe String
  , gameOver          :: Bool
  }
