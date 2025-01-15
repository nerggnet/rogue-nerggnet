-- src/Game/Types.hs
{-# LANGUAGE DeriveGeneric #-}

module Game.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), object, withObject, (.:), (.=))
import qualified Data.Aeson.Key as Key
import Linear.V2 (V2)

-- Custom JSON instances for V2
instance ToJSON a => ToJSON (V2 a)
instance FromJSON a => FromJSON (V2 a)

data Tile = Wall | Floor | Door | UpStair | DownStair deriving (Eq, Show, Generic)

instance ToJSON Tile
instance FromJSON Tile

data Direction = North | South | East | West | Up | Down deriving (Eq, Show, Generic)

instance ToJSON Direction
instance FromJSON Direction

data ItemCategory = Armor | Weapon | Healing | Special | Key deriving (Eq, Show, Generic)

instance ToJSON ItemCategory
instance FromJSON ItemCategory

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
  { mPosition :: V2 Int
  , mHealth   :: Int
  , mAttack   :: Int
  , mName     :: String
  , mXP       :: Int
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
  } deriving (Show, Eq, Generic)

instance ToJSON Item
instance FromJSON Item

data DoorEntity = DoorEntity
  { dePosition :: V2 Int
  , deLocked   :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON DoorEntity
instance FromJSON DoorEntity

data Trigger = Trigger
  { triggerCondition   :: GameState -> Bool -- Condition for activation
  , triggerActions     :: [Action]          -- Actions to execute
  , triggerDescription :: String            -- For debugging/logging
  }

data TriggerData
  = TriggerCoordinates (Int, Int)
  | TriggerString String
  deriving (Show)

data TriggerType = TriggerType
  { triggerTypeName :: String
  , triggerData     :: Maybe TriggerData
  }

data SerializableTrigger = SerializableTrigger
  { actions     :: [Action]
  , description :: String
  }
  deriving (Show, Generic)

instance ToJSON SerializableTrigger
instance FromJSON SerializableTrigger

data Action
  = SpawnItem String (V2 Int)        -- Item name and position
  | SpawnMonster String (V2 Int)     -- Monster name and position
  | UnlockDoor (V2 Int)              -- Position of the door
  | ShiftTile (V2 Int) Tile          -- Position and new tile type
  | TransportPlayer (V2 Int)         -- Target position for the player
  | DisplayMessage String            -- Message to display
  deriving (Show, Eq, Generic)

instance ToJSON Action
instance FromJSON Action

data World = World
  { mapGrid    :: [[Tile]]
  , monsters   :: [Monster]
  , npcs       :: [NPC]
  , items      :: [Item]
  , doors      :: [DoorEntity]
  , triggers   :: [Trigger]
  , serializedTriggers :: [SerializableTrigger]
  , visibility :: [[Bool]]
  , discovered :: [[Bool]]
  } deriving (Generic)

instance ToJSON World where
  toJSON world =
    object
      [ Key.fromString "mapGrid" .= mapGrid world
      , Key.fromString "monsters" .= monsters world
      , Key.fromString "npcs" .= npcs world
      , Key.fromString "items" .= items world
      , Key.fromString "doors" .= doors world
      , Key.fromString "serializedTriggers" .= serializedTriggers world
      , Key.fromString "visibility" .= visibility world
      , Key.fromString "discovered" .= discovered world
      ]

instance FromJSON World where
  parseJSON = withObject "World" $ \v -> do
    serialized <- v .: Key.fromString "serializedTriggers"
    World
      <$> v .: Key.fromString "mapGrid"
      <*> v .: Key.fromString "monsters"
      <*> v .: Key.fromString "npcs"
      <*> v .: Key.fromString "items"
      <*> v .: Key.fromString "doors"
      <*> pure [] -- Triggers will be initialized separately
      <*> pure serialized
      <*> v .: Key.fromString "visibility"
      <*> v .: Key.fromString "discovered"

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
  } deriving (Generic)

instance ToJSON GameState
instance FromJSON GameState
