-- src/Game/State.hs
module Game.State where

import Game.Types
import Game.GridUtils (gridLookup)
import qualified File.Types as FT
import Linear.V2 (V2(..))
import System.Random (StdGen)
import Data.Bifunctor (first)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)

-- | Everything wrong with a configuration file.
--
-- A list rather than a single message so that one run reports every problem
-- it can find, instead of making the author fix them one at a time.
type Problems = [String]

-- A single problem
problem :: String -> Either Problems a
problem msg = Left [msg]

-- | Either, with an Applicative that keeps the problems from both sides
-- instead of stopping at the first.
--
-- Checks that do not depend on each other are combined through this, so that
-- a world file with a bad item on one level and a bad map on another reports
-- both rather than making the author find them one run at a time.
newtype Validation a = Validation (Either Problems a)

instance Functor Validation where
  fmap f (Validation e) = Validation (fmap f e)

instance Applicative Validation where
  pure = Validation . Right
  Validation f <*> Validation x = Validation $ case (f, x) of
    (Left a, Left b)   -> Left (a ++ b)
    (Left a, _)        -> Left a
    (_, Left b)        -> Left b
    (Right g, Right y) -> Right (g y)

-- Run independent checks, keeping every problem any of them found
checkAll :: Validation a -> Either Problems a
checkAll (Validation e) = e

-- Combine results, keeping the problems from all of them
collect :: [Either Problems a] -> Either Problems [a]
collect = checkAll . traverse Validation

-- Say where a problem was found
inContext :: String -> Either Problems a -> Either Problems a
inContext what = first (map ((what ++ ": ") ++))

-- | The help, a page at a time.
--
-- Split up so that it fits a small terminal: every key the game responds to
-- belongs on one of these pages, and all of them together do not fit an
-- 80x24 screen at once. Blank separators are a space, not "", which has no
-- height to render.
helpPages :: [(String, [String])]
helpPages =
  [ ( "Moving and acting"
    , [ "w or k     Move up"
      , "s or j     Move down"
      , "a or h     Move left"
      , "d or l     Move right"
      , "<          Ascend the stairs you are on"
      , ">          Descend the stairs you are on"
      , "g          Pick up what you are standing on"
      , "u          Use or equip an item"
      , "x          Drop an item"
      , " "
      , "Walk into a monster to attack it,"
      , "or into an NPC to talk to them."
      ]
    )
  , ( "Choosing and aiming"
    , [ "a b c ...  Choose the item with that letter"
      , "Esc        Cancel without choosing"
      , " "
      , "Using a Range item starts aiming. Monsters"
      , "you can see are lettered on the map; press"
      , "a letter to shoot that one, or Esc to stop."
      ]
    )
  , ( "Commands"
    , [ ":          Start typing a command"
      , "Enter      Run it"
      , "Backspace  Rub out a character"
      , "Esc        Abandon it"
      , " "
      , ":q         Save and quit"
      , ":restart   Start a new dungeon"
      , ":heal      Cheat: back to full health"
      , ":super     Cheat: become very strong"
      ]
    )
  ]

-- Move to the next help page, closing the help after the last one
nextHelpPage :: Int -> Int
nextHelpPage page
  | page >= length helpPages = 0
  | otherwise = page + 1

-- Default values for monster, fog radius, and inventory size
defaultMonsterRadius :: Int
defaultMonsterRadius = 4

defaultFogRadius :: Int
defaultFogRadius = 5

-- NPCs take a step every this many turns
npcMoveInterval :: Int
npcMoveInterval = 3

maxInventorySize :: Int
maxInventorySize = 15

-- How many log lines are kept, and how many of those the message pane shows
maxLogMessages :: Int
maxLogMessages = 10

visibleLogMessages :: Int
visibleLogMessages = 5

-- Initialize the game state
-- Build a new game from a freshly loaded configuration.
--
-- Anything wrong with the configuration is reported rather than thrown, so a
-- typo in the world file is a message about the world file instead of a crash.
newGame :: StdGen -> FT.GameConfig -> Either Problems GameState
newGame gen config = do
  (initialXPLevel, allWorlds) <- checkAll $
    (,) <$> Validation (firstOr "no \"xpLevels\" are defined" allXPLevels)
        <*> Validation (collect
              [ inContext ("level " ++ show ix) (transformFileWorld fileWorld)
              | (ix, fileWorld) <- zip [0 :: Int ..] (FT.levels config)
              ])
  initialWorld <- firstOr "no \"levels\" are defined" allWorlds
  startingPosition <- inContext "level 0" $
    maybe (problem "the map grid has no \"S\" tile for the player to start on")
          Right
          (findStartingPosition initialWorld)
  let initialPlayer = Player
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
        , legendPage = 0
        , keyPressCount = 0
        , lastInteractedNpc = Nothing
        , aimingState = Nothing
        , gameOver = False
        , gameWon = False
        , rng = gen
        , hiddenTurns = 0
        }
      updatedWorld = updateVisibility initialPlayer defaultFogRadius initialWorld
  pure initialState { levels = replaceLevel initialState 0 updatedWorld }
  where
    allXPLevels = transformXPLevels (FT.xpLevels config)

    firstOr :: String -> [a] -> Either Problems a
    firstOr msg = maybe (problem msg) Right . listToMaybe

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

-- The level the player is standing on
currentWorld :: GameState -> World
currentWorld state = levels state !! currentLevel state

-- Apply a change to the level the player is standing on
withCurrentWorld :: (World -> World) -> GameState -> GameState
withCurrentWorld f state =
  state { levels = replaceLevel state (currentLevel state) (f (currentWorld state)) }

-- Draw from the game's generator, keeping the advanced one for next time
withRandom :: (StdGen -> (a, StdGen)) -> GameState -> (a, GameState)
withRandom draw state =
  let (value, gen) = draw (rng state)
   in (value, state {rng = gen})

-- Swap in a new version of the level the player is standing on
setCurrentWorld :: World -> GameState -> GameState
setCurrentWorld world = withCurrentWorld (const world)

-- Transform a File.Types.MapLevel to Game.Types.World
transformFileWorld :: FT.MapLevel -> Either Problems World
transformFileWorld fileWorld = do
  ((rows, cols), itms, trggrs) <- checkAll $
    (,,)
      <$> Validation gridSize
      <*> Validation (collect
            [ inContext ("item " ++ show (FT.itemName i)) (transformItem i)
            | i <- FT.items fileWorld
            ])
      <*> Validation (collect
            [ inContext ("trigger " ++ show ix) (transformJSONTrigger t)
            | (ix, t) <- zip [0 :: Int ..] (FT.triggers fileWorld)
            ])
  -- This one genuinely depends on the triggers above having been built.
  checked <- validateTriggers trggrs (FT.items fileWorld) (FT.npcs fileWorld)
  pure World
    { mapGrid = map (map charToTile) grid
    , mapRows = rows
    , mapCols = cols
    , monsters = map transformMonster (FT.monsters fileWorld)
    , npcs = map transformNPC (FT.npcs fileWorld)
    , items = itms
    , doors = map transformDoorEntity (FT.doors fileWorld)
    , triggers = checked
    , visibility = initializeGrid False rows cols
    , discovered = initializeGrid False rows cols
    , discoveredCoords = []
    , tileOverrides = []
    , corpses = []
    }
  where
    grid = FT.mapGrid fileWorld

    -- Every row must be the same length. A short row is not caught anywhere
    -- else: mapCols comes from the first row, so walking onto the missing
    -- part of a later one would fail at the point the player reached it.
    gridSize = case grid of
      [] -> problem "the \"mapGrid\" is empty"
      (firstRow : _) ->
        let width = length firstRow
            ragged = [ y | (y, row) <- zip [0 :: Int ..] grid, length row /= width ]
         in if null ragged
              then Right (length grid, width)
              else problem $ "the \"mapGrid\" is ragged: row 0 is " ++ show width
                     ++ " characters wide, but row(s) "
                     ++ intercalate ", " (map show ragged) ++ " are not"

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
  , mInactive = fromMaybe False (FT.inactive fm)
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
transformXPLevels = map $ \fxp -> XPLevel
  { xpLevel = FT.xpLevel fxp
  , xpThreshold = FT.xpThreshold fxp
  , xpHealth = FT.xpHealth fxp
  , xpAttack = FT.xpAttack fxp
  , xpResistance = FT.xpResistance fxp
  }

-- Transform a File.Types.JSONItem to Game.Types.Item
transformItem :: FT.JSONItem -> Either Problems Item
transformItem fi = do
  category <- parseItemCategory (FT.itemCategory fi)
  uses <- validateItemUses category (FT.itemUses fi)
  effect <- validateItemEffect category (FT.itemEffect fi)
  pure Item
    { iName = FT.itemName fi
    , iDescription = FT.itemDescription fi
    , iPosition = uncurry V2 (FT.itemPosition fi)
    , iCategory = category
    , iEffectValue = FT.itemEffectValue fi
    , iHidden = FT.itemHidden fi
    , iInactive = FT.itemInactive fi
    , iUses = uses
    , iEffect = effect
    }

itemEffects :: [(String, ItemEffect)]
itemEffects =
  [ ("Keepsake", Keepsake), ("Empower", Empower), ("Fortify", Fortify)
  , ("Reveal", Reveal), ("Blink", Blink), ("Firestorm", Firestorm)
  , ("Regenerate", Regenerate), ("Lifesteal", Lifesteal), ("Revive", Revive)
  , ("Vanish", Vanish)
  ]

parseItemEffect :: String -> Either Problems ItemEffect
parseItemEffect name =
  maybe (problem $ "unknown \"itemEffect\" " ++ show name ++ "; expected one of "
                   ++ intercalate ", " (map (show . fst) itemEffects))
        Right
        (lookup name itemEffects)

-- A Special item does whatever its "itemEffect" says, and nothing otherwise,
-- so one without an effect is inert and almost certainly a mistake. Every
-- other category already has behaviour of its own, so declaring an effect
-- there would quietly do nothing.
validateItemEffect :: ItemCategory -> Maybe String -> Either Problems (Maybe ItemEffect)
validateItemEffect Special Nothing =
  problem $ "a Special item must declare an \"itemEffect\"; expected one of "
            ++ intercalate ", " (map (show . fst) itemEffects)
validateItemEffect Special (Just name) = Just <$> parseItemEffect name
validateItemEffect category (Just _) =
  problem $ "only a Special item can declare an \"itemEffect\", and this is a "
            ++ show category ++ " item"
validateItemEffect _ Nothing = Right Nothing

itemCategories :: [(String, ItemCategory)]
itemCategories =
  [ ("Armor", Armor), ("Weapon", Weapon), ("Range", Range)
  , ("Healing", Healing), ("Special", Special), ("Key", Key)
  ]

parseItemCategory :: String -> Either Problems ItemCategory
parseItemCategory name =
  maybe (problem $ "unknown \"itemCategory\" " ++ show name ++ "; expected one of "
                   ++ intercalate ", " (map (show . fst) itemCategories))
        Right
        (lookup name itemCategories)

-- Categories whose items are spent as they are used
consumableCategories :: [ItemCategory]
consumableCategories = [Healing, Key, Range]

-- An item with no use count is never consumed, which only makes sense for
-- equipment. A consumable without one would be usable forever.
validateItemUses :: ItemCategory -> Maybe Int -> Either Problems (Maybe Int)
validateItemUses category uses
  | category `elem` consumableCategories && isNothing uses =
      problem $ "a " ++ show category ++ " item must declare \"itemUses\"; "
                ++ "without one it is never used up"
  | otherwise = Right uses

-- Transform a File.Types.JSONDoorEntity to Game.Types.DoorEntity
transformDoorEntity :: FT.JSONDoorEntity -> DoorEntity
transformDoorEntity jsonDoor = DoorEntity
  { dePosition = uncurry V2 (FT.doorPosition jsonDoor)
  , deLocked   = FT.doorLocked jsonDoor
  , deKeyName  = FT.doorKeyName jsonDoor
  }

-- Transform a File.Types.JSONTrigger to Game.Types.Trigger
transformJSONTrigger :: FT.JSONTrigger -> Either Problems Trigger
transformJSONTrigger jsonTrigger = checkAll $
  Trigger
    <$> Validation (conditionOf jsonTrigger)
    <*> Validation (collect
          [ inContext ("action " ++ show ix) (transformJSONAction a)
          | (ix, a) <- zip [0 :: Int ..] (FT.actions jsonTrigger)
          ])
    <*> pure (FT.recurring jsonTrigger)

-- Build the firing condition described by a JSON trigger
conditionOf :: FT.JSONTrigger -> Either Problems TriggerCondition
conditionOf jsonTrigger = case FT.triggerType jsonTrigger of
  "position" ->
    case FT.target jsonTrigger of
      Just (x, y) -> Right (AtPosition (V2 x y))
      Nothing     -> problem "a \"position\" trigger needs a \"target\""
  "posAndItems" ->
    case (FT.target jsonTrigger, FT.requiredItems jsonTrigger) of
      (Just (x, y), Just reqItems) -> Right (AtPositionWithItems (V2 x y) reqItems)
      _ -> problem "a \"posAndItems\" trigger needs both a \"target\" and \"requiredItems\""
  "itemPickup" ->
    case FT.triggerItemName jsonTrigger of
      Just itemName -> Right (HasItem itemName)
      Nothing       -> problem "an \"itemPickup\" trigger needs a \"triggerItemName\""
  "npcTalked" ->
    case FT.triggerNpcName jsonTrigger of
      Just nName -> Right (TalkedToNpc nName)
      Nothing    -> problem "an \"npcTalked\" trigger needs a \"triggerNpcName\""
  "allMonstersDefeated" -> Right AllMonstersDefeated
  other -> problem $ "unknown \"triggerType\" " ++ show other
             ++ "; expected one of \"position\", \"posAndItems\", \"itemPickup\", "
             ++ "\"npcTalked\", \"allMonstersDefeated\""

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
transformJSONAction :: FT.JSONTriggerAction -> Either Problems Action
transformJSONAction jsonAction = case FT.actionType jsonAction of
  "spawnItem" ->
    case (FT.actionItemName jsonAction, FT.actionPosition jsonAction) of
      (Just name, Just (x, y)) -> Right (SpawnItem name (V2 x y))
      _ -> needs "spawnItem" ["actionItemName", "actionPosition"]
  "spawnMonster" ->
    case (FT.actionMonsterName jsonAction, FT.actionPosition jsonAction) of
      (Just name, Just (x, y)) -> Right (SpawnMonster name (V2 x y))
      _ -> needs "spawnMonster" ["actionMonsterName", "actionPosition"]
  "unlockDoor" ->
    case FT.actionPosition jsonAction of
      Just (x, y) -> Right (UnlockDoor (V2 x y))
      _ -> needs "unlockDoor" ["actionPosition"]
  "displayMessage" ->
    case FT.actionMessage jsonAction of
      Just msg -> Right (DisplayMessage msg)
      _ -> needs "displayMessage" ["actionMessage"]
  "shiftTile" ->
    case (FT.actionPosition jsonAction, FT.actionTileType jsonAction) of
      (Just (x, y), Just tileType) -> Right (ShiftTile (V2 x y) (charToTile tileType))
      _ -> needs "shiftTile" ["actionPosition", "actionTileType"]
  "transportPlayer" ->
    case FT.actionPosition jsonAction of
      Just (x, y) -> Right (TransportPlayer (V2 x y))
      _ -> needs "transportPlayer" ["actionPosition"]
  "consumeItem" ->
    case FT.actionItemName jsonAction of
      Just name -> Right (ConsumeItem name)
      _ -> needs "consumeItem" ["actionItemName"]
  "addToInventory" ->
    case FT.actionItemName jsonAction of
      Just name -> Right (AddToInventory name)
      _ -> needs "addToInventory" ["actionItemName"]
  "setGameWon" -> Right SetGameWon
  other -> problem $ "unknown \"actionType\" " ++ show other
  where
    needs what fields =
      problem $ "a " ++ show what ++ " action needs "
                ++ intercalate " and " (map show fields)

-- Reject triggers that refer to items or NPCs the level does not define
validateTriggers :: [Trigger] -> [FT.JSONItem] -> [FT.JSONNPC] -> Either Problems [Trigger]
validateTriggers trggrs triggerItems triggerNpcs =
  collect [ inContext ("trigger " ++ show ix) (validateTrigger t)
          | (ix, t) <- zip [0 :: Int ..] trggrs
          ]
  where
    itemNames = map FT.itemName triggerItems
    npcNames  = map FT.npcName triggerNpcs

    validateTrigger trigger = case triggerCondition trigger of
      HasItem itemName
        | itemName `notElem` itemNames ->
            problem $ "needs item " ++ show itemName ++ ", which this level does not define"
      AtPositionWithItems _ required
        | missing@(_:_) <- filter (`notElem` itemNames) required ->
            problem $ "needs item(s) " ++ intercalate ", " (map show missing)
                      ++ ", which this level does not define"
      TalkedToNpc nName
        | nName `notElem` npcNames ->
            problem $ "refers to NPC " ++ show nName ++ ", which this level does not define"
      _ -> Right trigger

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

-- The XP level entry the player is currently at. The table is looked up by
-- level number rather than indexed, so it need not be contiguous or ordered.
currentXPLevel :: GameState -> Maybe XPLevel
currentXPLevel state =
  find ((== playerXPLevel (player state)) . xpLevel) (xpLevels state)

-- Maximum health at the player's current XP level. Falls back to the health
-- they already have, so a missing entry can never heal them.
maxHealth :: GameState -> Int
maxHealth state = maybe (health (player state)) xpHealth (currentXPLevel state)

-- Helper function to now if all monsters on a level have been defeated
allMonstersDefeated :: GameState -> Bool
allMonstersDefeated state = all mInactive (monsters (currentWorld state))

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

-- Find the tile the player starts on, if the map marks one
findStartingPosition :: World -> Maybe (V2 Int)
findStartingPosition wrld =
  listToMaybe
    [ V2 x y
    | (y, row) <- zip [0 ..] (mapGrid wrld)
    , (x, tile) <- zip [0 ..] row
    , tile == Start
    ]
