-- src/Game/State.hs
{-# OPTIONS_GHC -Wno-x-partial #-}

module Game.State where

import Game.Types
import qualified File.Types as FT
import Linear.V2 (V2(..))
import Data.List (isInfixOf)
import Data.List.Extra (dropPrefix, replace)
import Data.Function ((&))

-- Default values for unarmed and unarmored player, plus default monster and fog radii
defaultMonsterRadius :: Int
defaultMonsterRadius = 4

defaultFogRadius :: Int
defaultFogRadius = 5

-- Initialize the game state
initGame :: Either FT.GameConfig GameState -> GameState
initGame (Right savedState) =
  -- Reinitialize triggers using serializedTriggers from the saved game
  let reinitializedLevels =
        map (\world -> world { triggers = map toRuntimeTrigger (serializedTriggers world) })
            (levels savedState)
  in savedState { levels = reinitializedLevels }

initGame (Left config) =
  -- Fresh game initialization
  let allWorlds = map transformFileWorld (FT.levels config)
      allXPLevels = transformXPLevels (FT.xpLevels config)
      initialXPLevel = head allXPLevels
      initialWorld = head allWorlds
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
        , levels = map initializeTriggers allWorlds
        , xpLevels = allXPLevels
        , currentLevel = 0
        , message = ["Welcome to Rogue!"]
        , commandBuffer = ""
        , commandMode = False
        , showLegend = False
        , keyPressCount = 0
        , lastInteractedNpc = Nothing
        , gameOver = False
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
    rows = length (mapGrid world)
    cols = length (head (mapGrid world))

    isVisible :: V2 Int -> V2 Int -> Bool
    isVisible src dest
      | manhattanDistance src dest > radius = False
      | otherwise = all (\point -> isPassable (mapGrid world) (doors world) point || point == src || point == dest)
                        (bresenhamLine src dest)

    isPassable :: [[Tile]] -> [DoorEntity] -> V2 Int -> Bool
    isPassable grid drs (V2 x y) =
      let inBounds = y >= 0 && y < length grid && x >= 0 && x < length (head grid)
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
transformFileWorld fileWorld = World
  { mapGrid = map (map charToTile) (FT.mapGrid fileWorld)
  , monsters = map transformMonster (FT.monsters fileWorld)
  , npcs = map transformNPC (FT.npcs fileWorld)
  , items = map transformItem (FT.items fileWorld)
  , doors = map transformDoorEntity (FT.doors fileWorld)
  , triggers = validateTriggers (map transformJSONTrigger (FT.triggers fileWorld)) (FT.items fileWorld) (FT.npcs fileWorld)
  , serializedTriggers = map transformToSerializableTrigger (FT.triggers fileWorld)
  , visibility = initializeGrid False (length $ FT.mapGrid fileWorld) (length $ head $ FT.mapGrid fileWorld)
  , discovered = initializeGrid False (length $ FT.mapGrid fileWorld) (length $ head $ FT.mapGrid fileWorld)
  }

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
transformItem fi = Item
  { iName = FT.itemName fi
  , iDescription = FT.itemDescription fi
  , iPosition = uncurry V2 (FT.itemPosition fi)
  , iCategory = case FT.itemCategory fi of
                  "Armor" -> Armor
                  "Weapon" -> Weapon
                  "Healing" -> Healing
                  "Special" -> Special
                  "Key" -> Key
                  _ -> error "Unknown category"
  , iEffectValue = FT.itemEffectValue fi
  , iHidden = FT.itemHidden fi
  , iInactive = FT.itemInactive fi
  }

-- Transform a File.Types.JSONDoorEntity to Game.Types.DoorEntity
transformDoorEntity :: FT.JSONDoorEntity -> DoorEntity
transformDoorEntity jsonDoor = DoorEntity
  { dePosition = uncurry V2 (FT.doorPosition jsonDoor)
  , deLocked   = FT.doorLocked jsonDoor
  }

transformToSerializableTrigger :: FT.JSONTrigger -> SerializableTrigger
transformToSerializableTrigger jsonTrigger =
  let normalizedDescription = case FT.triggerType jsonTrigger of
        "position" ->
          case FT.target jsonTrigger of
            Just (x, y) -> "Position trigger at (" ++ show x ++ "," ++ show y ++ ")"
            Nothing     -> "Position trigger at (unknown)"
        "itemPickup" ->
          case FT.triggerItemName jsonTrigger of
            Just itemName -> "Item pickup trigger for " ++ itemName
            Nothing       -> "Item pickup trigger for (unknown item)"
        "npcTalked" ->
          case FT.triggerNpcName jsonTrigger of
            Just nName -> "Talked to NPC " ++ nName
            Nothing      -> "Talked to NPC (unknown)"
        "allMonstersDefeated" ->
          "Trigger when all monsters are defeated"
        _ ->
          "Unknown trigger type"
  in SerializableTrigger
       { actions = map transformJSONAction (FT.actions jsonTrigger)
       , description = normalizedDescription
       }

initializeTriggers :: World -> World
initializeTriggers world =
  world { triggers = map toRuntimeTrigger (serializedTriggers world) }

toRuntimeTrigger :: SerializableTrigger -> Trigger
toRuntimeTrigger sTrigger =
 let desc = normalizeDescription (description sTrigger)
  in case parseTriggerType desc of
       Just (TriggerType "position" (Just (TriggerCoordinates (x, y)))) ->
         Trigger
         { triggerCondition = \state -> position (player state) == V2 x y
         , triggerActions = actions sTrigger
         , triggerDescription = desc
         }
       Just (TriggerType "itemPickup" (Just (TriggerString itemName))) ->
         Trigger
         { triggerCondition = \state -> any (\item -> iName item == itemName) (inventory (player state))
         , triggerActions = actions sTrigger
         , triggerDescription = desc
         }
       Just (TriggerType "npcTalked" (Just (TriggerString nName))) ->
         Trigger
         { triggerCondition = \state ->
             case lastInteractedNpc state of
               Just interactedNpc -> interactedNpc == nName
               Nothing -> False
         , triggerActions = actions sTrigger
         , triggerDescription = desc
         }
       Just (TriggerType "allMonstersDefeated" Nothing) ->
         Trigger
         { triggerCondition = allMonstersDefeated
         , triggerActions = actions sTrigger
         , triggerDescription = desc
         }
       _ -> error $ "Unknown or unsupported trigger type: " ++ desc

normalizeDescription :: String -> String
normalizeDescription desc =
  desc
    -- Remove unnecessary "Just" annotations
    & replace "Just \"" ""
    & replace "\"" ""

parseTriggerType :: String -> Maybe TriggerType
parseTriggerType desc =
    if "Position trigger at " `isInfixOf` desc then
      case extractCoordinates desc of
        Just coords -> Just $ TriggerType "position" (Just $ TriggerCoordinates coords)
        Nothing -> Nothing
    else if "Item pickup trigger for " `isInfixOf` desc then
      Just $ TriggerType "itemPickup" (Just $ TriggerString $ drop (length "Item pickup trigger for ") desc)
    else if "Talked to NPC " `isInfixOf` desc then
      Just $ TriggerType "npcTalked" (Just $ TriggerString $ drop (length "Talked to NPC ") desc)
    else if "Trigger when all monsters are defeated" `isInfixOf` desc then
      Just $ TriggerType "allMonstersDefeated" Nothing
    else
      Nothing

extractCoordinates :: String -> Maybe (Int, Int)
extractCoordinates desc =
    case reads (drop (length "Position trigger at ") desc) :: [((Int, Int), String)] of
      [(coords, "")] -> Just coords
      _ -> Nothing

extractData :: String -> String -> Maybe String
extractData prefix desc =
  let trimmed = drop (length prefix) desc
  in if null trimmed then Nothing else Just trimmed

-- Transform a File.Types.JSONTrigger to Game.Types.Trigger
transformJSONTrigger :: FT.JSONTrigger -> Trigger
transformJSONTrigger jsonTrigger = case FT.triggerType jsonTrigger of
  "position" -> Trigger
    { triggerCondition = \state ->
        case FT.target jsonTrigger of
          Just (x, y) -> position (player state) == V2 x y
          Nothing     -> False
    , triggerActions = map transformJSONAction (FT.actions jsonTrigger)
    , triggerDescription = "Position trigger at " ++ show (FT.target jsonTrigger)
    }
  "itemPickup" -> Trigger
    { triggerCondition = \state ->
        case FT.triggerItemName jsonTrigger of
          Just name -> any (\item -> iName item == name) (inventory (player state))
          Nothing   -> False
    , triggerActions = map transformJSONAction (FT.actions jsonTrigger)
    , triggerDescription = "Item pickup trigger for " ++ show (FT.triggerItemName jsonTrigger)
    }
  "npcTalked" -> Trigger
    { triggerCondition = \state ->
        case (lastInteractedNpc state, FT.triggerNpcName jsonTrigger) of
          (Just interacted, Just expected) -> interacted == expected
          _ -> False
    , triggerActions = map transformJSONAction (FT.actions jsonTrigger)
    , triggerDescription = "Talked to NPC " ++ show (FT.triggerNpcName jsonTrigger)
    }
  "allMonstersDefeated" -> Trigger
    { triggerCondition = allMonstersDefeated
    , triggerActions = map transformJSONAction (FT.actions jsonTrigger)
    , triggerDescription = "Trigger when all monsters on the level are defeated"
    }
  _ -> error $ "Unknown trigger type: " ++ FT.triggerType jsonTrigger

-- Convert JSONTriggerAction to Action
transformJSONAction :: FT.JSONTriggerAction -> Action
transformJSONAction jsonAction = case FT.actionType jsonAction of
  "spawnItem" ->
    case (FT.actionItemName jsonAction, FT.actionPosition jsonAction) of
      (Just name, Just (x, y)) -> SpawnItem name (V2 x y)
      _ -> error "Invalid spawnItem action"
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
  _ -> error $ "Unknown action type: " ++ FT.actionType jsonAction

validateTriggers :: [Trigger] -> [FT.JSONItem] -> [FT.JSONNPC] -> [Trigger]
validateTriggers trggrs triggerItems triggerNpcs = map validateTrigger trggrs
  where
    itemNames = map FT.itemName triggerItems
    npcNames = map FT.npcName triggerNpcs
    validateTrigger trigger@(Trigger { triggerCondition = _, triggerActions = _, triggerDescription = desc })
      | "itemPickup" `isInfixOf` desc =
          if any (\item -> item `isInfixOf` desc) itemNames
          then trigger
          else error $ "Trigger refers to an unknown item: " ++ desc
      | "npcTalked" `isInfixOf` desc =
          let npc = dropPrefix "Talked to NPC " desc
           in if npc `elem` npcNames
              then trigger
              else error $ "Trigger refers to unknown NPC: " ++ npc
      | otherwise = trigger

-- Helper function to now if all monsters on a level have been defeated
allMonstersDefeated :: GameState -> Bool
allMonstersDefeated state =
  null (monsters (levels state !! currentLevel state))

-- Convert a character to a Tile
charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile '.' = Floor
charToTile '+' = Door
charToTile '<' = UpStair
charToTile '>' = DownStair
charToTile 'S' = Start
charToTile _   = Floor -- Default to Floor for unknown characters.

-- Find the starting position (e.g., the first Floor tile)
findStartingPosition :: World -> V2 Int
findStartingPosition wrld =
  let grid = mapGrid wrld
  in case [(x, y) | (y, row) <- zip [0..] grid, (x, tile) <- zip [0..] row, tile == Start] of
       ((x, y):_) -> V2 x y
       _          -> V2 0 0 -- Default to top-left if no Floor tile is found.
