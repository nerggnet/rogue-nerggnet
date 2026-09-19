-- src/Game/State.hs
module Game.State where

import Game.Types
import Game.GridUtils (updateTile, gridLookup)
import qualified File.Types as FT
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)

-- Default values for monster, fog radius, and inventory size
defaultMonsterRadius :: Int
defaultMonsterRadius = 4

defaultFogRadius :: Int
defaultFogRadius = 5

maxInventorySize :: Int
maxInventorySize = 15

-- Initialize the game state
initGame :: Either FT.GameConfig GameState -> GameState
initGame (Right savedState) = savedState

initGame (Left config) =
  -- Fresh game initialization
  let allWorlds = map transformFileWorld (FT.levels config)
      allXPLevels = transformXPLevels (FT.xpLevels config)
      initialXPLevel = case allXPLevels of
                         []    -> error "No XP levels found"
                         (l:_) -> l
      initialWorld = case allWorlds of
                       []    -> error "No dungeon levels found"
                       (w:_) -> w
      startingPosition = findStartingPosition initialWorld
      initialPlayer = Player
        { position = startingPosition
        , health = xpHealth initialXPLevel
        , baseAttack = xpAttack initialXPLevel
        , baseResistance = xpResistance initialXPLevel
        , attack = xpAttack initialXPLevel
        , resistance = xpResistance initialXPLevel
        , xp = 0
        , playerXPLevel = 1
        , inventory = []
        , equippedWeapon = Nothing
        , equippedArmor = Nothing
        }
      initialState = GameState
        { player = initialPlayer
        , levels = allWorlds
        , xpLevels = allXPLevels
        , currentLevel = 0
        , message = ["Welcome to Rogue nerggnet!"]
        , commandBuffer = ""
        , commandMode = False
        , commandToExecute = False
        , inventoryMode = Nothing
        , showLegend = False
        , keyPressCount = 0
        , lastInteractedNpc = Nothing
        , aimingState = Nothing
        , gameOver = False
        , gameWon = False
        }
      updatedWorld = updateVisibility initialPlayer defaultFogRadius initialWorld
  in initialState { levels = replaceLevel initialState 0 updatedWorld }

-- Update what the player sees of the map
updateVisibility :: Player -> Int -> World -> World
updateVisibility plyr radius world =
  let pos = position plyr
      updatedVisibility = [ [isVisible pos (V2 x y) | x <- [0..cols-1]] | y <- [0..rows-1] ]
      updatedDiscovered = zipWith (zipWith (||)) updatedVisibility (discovered world)
  in world { visibility = updatedVisibility, discovered = updatedDiscovered }
  where
    rows = mapRows world
    cols = mapCols world

    isVisible :: V2 Int -> V2 Int -> Bool
    isVisible src dest
      | manhattanDistance src dest > radius = False
      | otherwise = all (\point -> isPassable (mapGrid world) (doors world) point || point == src || point == dest)
                        (bresenhamLine src dest)

    isPassable :: [[Tile]] -> [DoorEntity] -> V2 Int -> Bool
    isPassable grid drs (V2 x y) =
      let inBounds = y >= 0 && y < rows && x >= 0 && x < cols
          isDoor = any (\door -> dePosition door == V2 x y && deLocked door) drs
      in inBounds && not isDoor && grid !! y !! x /= Wall

bresenhamLine :: V2 Int -> V2 Int -> [V2 Int]
bresenhamLine (V2 x0 y0) (V2 x1 y1) =
  let dx = abs (x1 - x0)
      dy = abs (y1 - y0)
      sx = if x0 < x1 then 1 else -1
      sy = if y0 < y1 then 1 else -1
      go x y err
        | x == x1 && y == y1 = [V2 x y]
        | otherwise =
            let (newX, newY, newErr) =
                  if err > -dx
                  then if err < dy
                       then (x + sx, y + sy, err - dy + dx)
                       else (x + sx, y, err - dy)
                  else (x, y + sy, err + dx)
            in V2 x y : go newX newY newErr
  in go x0 y0 (dx - dy)

-- Manhattan distance between two points
manhattanDistance :: V2 Int -> V2 Int -> Int
manhattanDistance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- Replace the current level with an updated one
replaceLevel :: GameState -> Int -> World -> [World]
replaceLevel state levelIndex newWorld =
  take levelIndex (levels state) ++ [newWorld] ++ drop (levelIndex + 1) (levels state)

-- Transform a File.Types.MapLevel to Game.Types.World
transformFileWorld :: FT.MapLevel -> World
transformFileWorld fileWorld =
  let rows = length (FT.mapGrid fileWorld)
      cols = case FT.mapGrid fileWorld of
               []    -> error "Incorrectly formatted dungeon map"
               (r:_) -> length r
   in World
        { mapGrid = map (map charToTile) (FT.mapGrid fileWorld)
        , mapRows = rows
        , mapCols = cols
        , monsters = map transformMonster (FT.monsters fileWorld)
        , npcs = map transformNPC (FT.npcs fileWorld)
        , items = map transformItem (FT.items fileWorld)
        , doors = map transformDoorEntity (FT.doors fileWorld)
        , triggers = validateTriggers (map transformJSONTrigger (FT.triggers fileWorld))
                                      (FT.items fileWorld) (FT.npcs fileWorld)
        , visibility = initializeGrid False rows cols
        , discovered = initializeGrid False rows cols
        , discoveredCoords = []
        , tileOverrides = []
        , corpses = []
        }

-- Apply overrides to the base grid
applyTileOverrides :: [[Tile]] -> [(V2 Int, Tile)] -> [[Tile]]
applyTileOverrides grid overrides =
  foldl (\g (pos, tile) -> updateTile g (pos ^. _x, pos ^. _y) tile) grid overrides

initializeGrid :: a -> Int -> Int -> [[a]]
initializeGrid value rows cols = replicate rows (replicate cols value)

-- Transform a File.Types.JSONMonster to Game.Types.Monster
transformMonster :: FT.JSONMonster -> Monster
transformMonster fm = Monster
  { mPosition = uncurry V2 (FT.position fm)
  , mHealth = FT.health fm
  , mAttack = FT.attack fm
  , mName = FT.name fm
  , mXP = FT.xp fm
  , mInactive = maybe False id (FT.inactive fm)
  , mAttackWait = True
  }

-- Transform a File.Types.JSONNPC to Game.Types.NPC
transformNPC :: FT.JSONNPC -> NPC
transformNPC fnpc = NPC
  { npcName = FT.npcName fnpc
  , npcPosition = uncurry V2 (FT.npcPosition fnpc)
  , npcMessage = FT.npcMessage fnpc
  , npcPreferredDirection = Nothing
  }

-- Transform a File.Types.XPLevel to Game.Types.XPLevel
transformXPLevels :: [FT.XPLevel] -> [XPLevel]
transformXPLevels fxps = map (\fxp -> XPLevel
  { xpLevel = FT.xpLevel fxp
  , xpThreshold = FT.xpThreshold fxp
  , xpHealth = FT.xpHealth fxp
  , xpAttack = FT.xpAttack fxp
  , xpResistance = FT.xpResistance fxp
  }) fxps

-- Transform a File.Types.JSONItem to Game.Types.Item
transformItem :: FT.JSONItem -> Item
transformItem fi =
  let category = parseItemCategory (FT.itemName fi) (FT.itemCategory fi)
   in Item
        { iName = FT.itemName fi
        , iDescription = FT.itemDescription fi
        , iPosition = uncurry V2 (FT.itemPosition fi)
        , iCategory = category
        , iEffectValue = FT.itemEffectValue fi
        , iHidden = FT.itemHidden fi
        , iInactive = FT.itemInactive fi
        , iUses = validateItemUses category (FT.itemName fi) (FT.itemUses fi)
        }

parseItemCategory :: String -> String -> ItemCategory
parseItemCategory _ "Armor"   = Armor
parseItemCategory _ "Weapon"  = Weapon
parseItemCategory _ "Range"   = Range
parseItemCategory _ "Healing" = Healing
parseItemCategory _ "Special" = Special
parseItemCategory _ "Key"     = Key
parseItemCategory itemName other =
  error $ "Unknown item category \"" ++ other ++ "\" for item \"" ++ itemName ++ "\""

-- Categories whose items are spent as they are used
consumableCategories :: [ItemCategory]
consumableCategories = [Healing, Key, Range]

-- An item with no use count is never consumed, which only makes sense for
-- equipment. A consumable without one would be usable forever.
validateItemUses :: ItemCategory -> String -> Maybe Int -> Maybe Int
validateItemUses category itemName uses
  | category `elem` consumableCategories && isNothing uses =
      error $ show category ++ " item \"" ++ itemName ++ "\" must declare \"itemUses\""
  | otherwise = uses

-- Transform a File.Types.JSONDoorEntity to Game.Types.DoorEntity
transformDoorEntity :: FT.JSONDoorEntity -> DoorEntity
transformDoorEntity jsonDoor = DoorEntity
  { dePosition = uncurry V2 (FT.doorPosition jsonDoor)
  , deLocked   = FT.doorLocked jsonDoor
  , deKeyName  = FT.doorKeyName jsonDoor
  }

-- Transform a File.Types.JSONTrigger to Game.Types.Trigger
transformJSONTrigger :: FT.JSONTrigger -> Trigger
transformJSONTrigger jsonTrigger = Trigger
  { triggerCondition = conditionOf jsonTrigger
  , triggerActions   = map transformJSONAction (FT.actions jsonTrigger)
  , triggerRecurring = FT.recurring jsonTrigger
  }

-- Build the firing condition described by a JSON trigger
conditionOf :: FT.JSONTrigger -> TriggerCondition
conditionOf jsonTrigger = case FT.triggerType jsonTrigger of
  "position" ->
    case FT.target jsonTrigger of
      Just (x, y) -> AtPosition (V2 x y)
      Nothing     -> error "A \"position\" trigger needs a \"target\""
  "posAndItems" ->
    case (FT.target jsonTrigger, FT.requiredItems jsonTrigger) of
      (Just (x, y), Just reqItems) -> AtPositionWithItems (V2 x y) reqItems
      _ -> error "A \"posAndItems\" trigger needs both a \"target\" and \"requiredItems\""
  "itemPickup" ->
    case FT.triggerItemName jsonTrigger of
      Just itemName -> HasItem itemName
      Nothing       -> error "An \"itemPickup\" trigger needs a \"triggerItemName\""
  "npcTalked" ->
    case FT.triggerNpcName jsonTrigger of
      Just nName -> TalkedToNpc nName
      Nothing    -> error "An \"npcTalked\" trigger needs a \"triggerNpcName\""
  "allMonstersDefeated" -> AllMonstersDefeated
  other -> error $ "Unknown trigger type: " ++ other

-- Interpret a trigger condition against the current game state
evalTriggerCondition :: TriggerCondition -> GameState -> Bool
evalTriggerCondition (AtPosition pos) state =
  position (player state) == pos
evalTriggerCondition (AtPositionWithItems pos required) state =
  position (player state) == pos && all carried required
  where
    carried n = any ((== n) . iName) (inventory (player state))
evalTriggerCondition (HasItem itemName) state =
  any ((== itemName) . iName) (inventory (player state))
evalTriggerCondition (TalkedToNpc nName) state =
  lastInteractedNpc state == Just nName
evalTriggerCondition AllMonstersDefeated state =
  allMonstersDefeated state

-- Convert JSONTriggerAction to Action
transformJSONAction :: FT.JSONTriggerAction -> Action
transformJSONAction jsonAction = case FT.actionType jsonAction of
  "spawnItem" ->
    case (FT.actionItemName jsonAction, FT.actionPosition jsonAction) of
      (Just name, Just (x, y)) -> SpawnItem name (V2 x y)
      _ -> error "Invalid spawnItem action"
  "spawnMonster" ->
    case (FT.actionMonsterName jsonAction, FT.actionPosition jsonAction) of
      (Just name, Just (x, y)) -> SpawnMonster name (V2 x y)
      _ -> error "Invalid spawnMonster action"
  "unlockDoor" ->
    case FT.actionPosition jsonAction of
      Just (x, y) -> UnlockDoor (V2 x y)
      _ -> error "Invalid unlockDoor action"
  "displayMessage" ->
    case FT.actionMessage jsonAction of
      Just msg -> DisplayMessage msg
      _ -> error "Invalid displayMessage action"
  "shiftTile" ->
    case (FT.actionPosition jsonAction, FT.actionTileType jsonAction) of
      (Just (x, y), Just tileType) -> ShiftTile (V2 x y) (charToTile tileType)
      _ -> error "Invalid shiftTile action"
  "transportPlayer" ->
    case FT.actionPosition jsonAction of
      Just (x, y) -> TransportPlayer (V2 x y)
      _ -> error "Invalid transportPlayer action"
  "consumeItem" ->
    case FT.actionItemName jsonAction of
      Just name -> ConsumeItem name
      _ -> error "Invalid consumeItem action"
  "addToInventory" ->
    case FT.actionItemName jsonAction of
      Just name -> AddToInventory name
      _ -> error "Invalid addToInventory action"
  "setGameWon" -> SetGameWon
  _ -> error $ "Unknown action type: " ++ FT.actionType jsonAction

-- Reject triggers that refer to items or NPCs the level does not define
validateTriggers :: [Trigger] -> [FT.JSONItem] -> [FT.JSONNPC] -> [Trigger]
validateTriggers trggrs triggerItems triggerNpcs = map validateTrigger trggrs
  where
    itemNames = map FT.itemName triggerItems
    npcNames  = map FT.npcName triggerNpcs

    validateTrigger trigger = case triggerCondition trigger of
      HasItem itemName
        | itemName `notElem` itemNames ->
            error $ "Trigger refers to an unknown item: " ++ itemName
      AtPositionWithItems _ required
        | missing@(_:_) <- filter (`notElem` itemNames) required ->
            error $ "Trigger refers to unknown items: " ++ intercalate ", " missing
      TalkedToNpc nName
        | nName `notElem` npcNames ->
            error $ "Trigger refers to an unknown NPC: " ++ nName
      _ -> trigger

-- Is a position currently lit for the player? Out of bounds counts as unseen.
isVisibleAt :: World -> V2 Int -> Bool
isVisibleAt world pos = fromMaybe False (gridLookup (visibility world) pos)

-- Active monsters the player can see, labelled from 'a' for ranged targeting.
-- The map and the targeting logic share this so that their letters agree.
visibleMonsters :: World -> [(Char, Monster)]
visibleMonsters world =
  zip ['a'..] (filter onScreen (filter (not . mInactive) (monsters world)))
  where
    onScreen = isVisibleAt world . mPosition

-- Helper function to now if all monsters on a level have been defeated
allMonstersDefeated :: GameState -> Bool
allMonstersDefeated state =
  null (filter (not . mInactive) (monsters (levels state !! currentLevel state)))

-- Convert a character to a Tile (and back again)
charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile '.' = Floor
charToTile '+' = Door
charToTile '<' = UpStair
charToTile '>' = DownStair
charToTile 'S' = Start
charToTile _   = Floor -- Default to Floor for unknown characters.

tileToChar :: Tile -> Char
tileToChar Wall      = '#'
tileToChar Floor     = '.'
tileToChar Door      = '+'
tileToChar UpStair   = '<'
tileToChar DownStair = '>'
tileToChar Start     = 'S'

-- Find the starting position (e.g., the first Floor tile)
findStartingPosition :: World -> V2 Int
findStartingPosition wrld =
  let grid = mapGrid wrld
  in case [(x, y) | (y, row) <- zip [0..] grid, (x, tile) <- zip [0..] row, tile == Start] of
       ((x, y):_) -> V2 x y
       _          -> V2 0 0 -- Default to top-left if no Floor tile is found.
