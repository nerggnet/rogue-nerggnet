-- src/Game/State.hs
module Game.State where

import Game.Types
import Game.GridUtils (gridLookup, orthogonal)
import qualified File.Types as FT
import Linear.V2 (V2(..))
import System.Random (StdGen)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.List (find, intercalate, nub)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
  , ( "Shafts and getting out"
    , [ "^          A shaft, with daylight behind it"
      , " "
      , "Standing on one costs nothing. Use a rope"
      , "there to climb to the floor above; from the"
      , "first floor, that is out of the dungeon and"
      , "the end of the run. The rope is spent."
      , " "
      , "A run is scored on how deep you went and"
      , "what you carried out, so leaving is a"
      , "decision, not a failure."
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
      , ":scores    Runs finished so far"
      ]
    )
  ]

-- | What an item does, in a few words.
--
-- Every item carries a description, and for a long time the game never
-- showed it anywhere: an inventory of names told the player nothing about
-- what any of them was for, and a Special can only be found out by using
-- it, which for half of them spends it. This is the mechanical half, said
-- plainly; the description is the flavour and is shown on picking it up.
--
-- It lives here rather than in the UI because it is a statement about the
-- rules, and the rules are not allowed to depend on a terminal.
whatItDoes :: Item -> String
whatItDoes itm = case iCategory itm of
  Weapon  -> plus "attack while wielded"
  Armor   -> plus "resistance while worn"
  Healing -> "heals " ++ show value
  Key     -> "opens the door it matches"
  Range   -> "shot from a distance, " ++ plus "damage"
  Special -> maybe "nothing at all" effectDoes (iEffect itm)
  where
    value = iEffectValue itm
    plus what = "+" ++ show value ++ " " ++ what
    once = " (once)"
    effectDoes effect = case effect of
      Keepsake   -> "carried for its own sake"
      Empower    -> plus "attack, for good" ++ once
      Fortify    -> plus "resistance, for good" ++ once
      Reveal     -> "maps the whole floor" ++ once
      Blink      -> "puts you elsewhere on the floor" ++ once
      Firestorm  -> show value ++ " damage to all in sight" ++ once
      Regenerate -> "heals " ++ show value ++ " a turn while carried"
      Lifesteal  -> "returns " ++ show value ++ "% of damage dealt, while carried"
      Revive     -> "saves you from one death, then burns up"
      Vanish     -> "unseen for " ++ show value ++ " turns" ++ once
      Escape     -> "climbs a shaft to the floor above" ++ once

-- Move to the next help page, closing the help after the last one
nextHelpPage :: Int -> Int
nextHelpPage page
  | page >= length helpPages = 0
  | otherwise = page + 1

-- Default values for monster, fog radius, and inventory size

-- How far a monster will follow the player, counted in steps it would
-- actually have to walk rather than as the crow flies, so one on the far
-- side of a wall stays where it is.
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
  -- Every level is built before any is judged, so a fault on one does not
  -- hide the faults on the others, or in the joins between them.
  checkAll $
    Validation (void (collect
      [ inContext ("level " ++ show ix)
          (checkAll (Validation (checkLevelReachable world)
                       *> Validation (checkTriggerActions world)))
      | (ix, world) <- zip [0 :: Int ..] allWorlds
      ]))
      *> Validation (checkStairsMeet allWorlds)
      *> Validation (checkDoorsOpenable allWorlds)
      *> Validation (checkTriggerItems allWorlds)
      *> Validation (checkShaftsClimbable allWorlds)
      *> Validation (checkItemsAgree allWorlds)
      *> Validation (checkShootersAreVisible allWorlds)
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
        , turnCount = 0
        , scoreboard = []
        , showScores = False
        , lastInteractedNpc = Nothing
        , aimingState = Nothing
        , gameOver = False
        , gameWon = False
        , rng = gen
        , hiddenTurns = 0
        , defeatedMonsters = []
        , deepestLevel = 0
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
      updatedVisibility = [ [seesFrom world radius pos (V2 x y) | x <- [0..cols-1]] | y <- [0..rows-1] ]
      updatedDiscovered = zipWith (zipWith (||)) updatedVisibility (discovered world)
  in world { visibility = updatedVisibility, discovered = updatedDiscovered }
  where
    cols = mapCols world
    rows = mapRows world

-- | Whether one tile can be seen from another, within a range.
--
-- Walls and locked doors block the line; the two ends never block it
-- themselves, or nothing could see out of a doorway. This is the player's
-- fog of war and a monster's aim both: an archer that could shoot through a
-- wall, or at something it could not see, would not be playing the same
-- game as the player.
seesFrom :: World -> Int -> V2 Int -> V2 Int -> Bool
seesFrom world radius src dest
  | manhattanDistance src dest > radius = False
  | otherwise = all clear (bresenhamLine src dest)
  where
    clear point = point == src || point == dest || isPassable point
    isPassable (V2 x y) =
      let inBounds = y >= 0 && y < mapRows world && x >= 0 && x < mapCols world
          shut = any (\door -> dePosition door == V2 x y && deLocked door) (doors world)
       in inBounds && not shut && mapGrid world !! y !! x /= Wall

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
  checked <- validateTriggers trggrs (FT.npcs fileWorld) (FT.monsters fileWorld)
  let built = World
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
  pure built
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
  , mRange = FT.range fm
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
    , iValue = fromMaybe 0 (FT.itemValue fi)
    }

itemEffects :: [(String, ItemEffect)]
itemEffects =
  [ ("Keepsake", Keepsake), ("Empower", Empower), ("Fortify", Fortify)
  , ("Reveal", Reveal), ("Blink", Blink), ("Firestorm", Firestorm)
  , ("Regenerate", Regenerate), ("Lifesteal", Lifesteal), ("Revive", Revive)
  , ("Vanish", Vanish), ("Escape", Escape)
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
  "monsterDefeated" ->
    case FT.triggerMonsterName jsonTrigger of
      Just mname -> Right (MonsterDefeated mname)
      Nothing    -> problem "a \"monsterDefeated\" trigger needs a \"triggerMonsterName\""
  "allMonstersDefeated" -> Right AllMonstersDefeated
  other -> problem $ "unknown \"triggerType\" " ++ show other
             ++ "; expected one of \"position\", \"posAndItems\", \"itemPickup\", "
             ++ "\"npcTalked\", \"monsterDefeated\", \"allMonstersDefeated\""

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
evalTriggerCondition (MonsterDefeated mname) state =
  mname `elem` defeatedMonsters state
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
  "harmPlayer" ->
    case FT.actionAmount jsonAction of
      Just n -> Right (HarmPlayer n)
      _ -> needs "harmPlayer" ["actionAmount"]
  "healPlayer" ->
    case FT.actionAmount jsonAction of
      Just n -> Right (HealPlayer n)
      _ -> needs "healPlayer" ["actionAmount"]
  "setGameWon" -> Right SetGameWon
  other -> problem $ "unknown \"actionType\" " ++ show other
  where
    needs what fields =
      problem $ "a " ++ show what ++ " action needs "
                ++ intercalate " and " (map show fields)

-- Reject triggers that refer to items or NPCs the level does not define
-- An NPC or a monster a trigger names has to be on the level with it: you
-- talk to one and kill the other where it stands. Items are checked across
-- levels instead, by checkTriggerItems, since the player carries them down.
validateTriggers :: [Trigger] -> [FT.JSONNPC] -> [FT.JSONMonster] -> Either Problems [Trigger]
validateTriggers trggrs triggerNpcs triggerMonsters =
  collect [ inContext ("trigger " ++ show ix) (validateTrigger t)
          | (ix, t) <- zip [0 :: Int ..] trggrs
          ]
  where
    npcNames  = map FT.npcName triggerNpcs
    -- Spawn templates count: a boss is usually inactive until a trigger
    -- calls it up, and a trigger may well wait on that same boss dying.
    monsterNames = map FT.name triggerMonsters

    validateTrigger trigger = case triggerCondition trigger of
      TalkedToNpc nName
        | nName `notElem` npcNames ->
            problem $ "refers to NPC " ++ show nName ++ ", which this level does not define"
      MonsterDefeated mname
        | mname `notElem` monsterNames ->
            problem $ "waits on the monster " ++ show mname
                      ++ ", which this level does not define"
      _ -> Right trigger

-- What the player is carrying, counted in treasure.
--
-- The run is judged on what comes back out, so this is the number that makes
-- one attempt comparable with another, alongside how far down it got.
treasureCarried :: GameState -> Int
treasureCarried state = sum (map iValue (inventory (player state)))

-- Fail with all of these at once, or succeed
noProblems :: Problems -> Either Problems ()
noProblems [] = Right ()
noProblems problems = Left problems

showPos :: V2 Int -> String
showPos (V2 x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

-- Where a given tile appears on the map
tilesOf :: Tile -> World -> [V2 Int]
tilesOf wanted world =
  [ V2 x y
  | (y, row) <- zip [0 ..] (mapGrid world)
  , (x, tile) <- zip [0 ..] row
  , tile == wanted
  ]

-- Where the player arrives on a level: the start tile, or the stairs up
entryTile :: World -> Maybe (V2 Int)
entryTile world = listToMaybe (tilesOf Start world ++ tilesOf UpStair world)

-- Every tile that can be walked to from here.
--
-- Doors count as open, locked or not: a locked door is a puzzle to be solved
-- with a key, not a wall. Only walls and the edge of the map stop the walk.
reachableFrom :: World -> V2 Int -> Set.Set (V2 Int)
reachableFrom world start = walk (Set.singleton start) [start]
  where
    walk seen [] = seen
    walk seen (pos : rest) =
      let found = [next | next <- orthogonal pos, walkable next, not (Set.member next seen)]
       in walk (foldr Set.insert seen found) (found ++ rest)
    walkable = standable world

-- Somewhere something could stand: on the map, and not inside a wall.
standable :: World -> V2 Int -> Bool
standable world pos = gridLookup (mapGrid world) pos `notElem` [Nothing, Just Wall]

-- Everything placed on a level has to be somewhere the player can get to.
--
-- A map is drawn by hand, and one wall in the wrong place quietly strands a
-- room full of things nobody will ever see. Inactive items are left out: a
-- trigger may put one straight into the player's pack, so where it sits on
-- the map means nothing.
checkLevelReachable :: World -> Either Problems ()
checkLevelReachable world = case entryTile world of
  Nothing ->
    problem "there is no \"S\" start tile and no \"<\" stairs up, so the player could never arrive"
  Just entry ->
    let reached = reachableFrom world entry
        stranded pos = not (Set.member pos reached)
     in noProblems $ concat
          [ [ "the stairs down at " ++ showPos pos ++ " cannot be reached"
            | pos <- tilesOf DownStair world, stranded pos ]
          , [ "the monster " ++ show (mName m) ++ " at " ++ showPos (mPosition m) ++ " cannot be reached"
            | m <- monsters world, not (mInactive m), stranded (mPosition m) ]
          , [ "the item " ++ show (iName i) ++ " at " ++ showPos (iPosition i) ++ " cannot be reached"
            | i <- items world, not (iInactive i), stranded (iPosition i) ]
          , [ "the NPC " ++ show (npcName n) ++ " at " ++ showPos (npcPosition n) ++ " cannot be reached"
            | n <- npcs world, stranded (npcPosition n) ]
          , [ "the door at " ++ showPos (dePosition d) ++ " is inside a wall"
            | d <- doors world, gridLookup (mapGrid world) (dePosition d) == Just Wall ]
            -- A door entity locks and unlocks; the tile under it is what is
            -- drawn. Put one on a floor tile and it stops the player dead
            -- with nothing on the screen to say why.
          , [ "the door at " ++ showPos (dePosition d) ++ " is not drawn as a door;"
              ++ " the tile there is " ++ maybe "off the map" show tile
            | d <- doors world
            , let tile = gridLookup (mapGrid world) (dePosition d)
            , tile /= Just Wall
            , tile /= Just Door ]
          ]

-- What a trigger's actions reach for has to be there.
--
-- These fail quietly at the moment the player springs them: a spawn with no
-- template writes a line to the log and carries on, and one that names a
-- position nothing is placed at does not even do that. Either way the
-- author finds out years later, if at all.
checkTriggerActions :: World -> Either Problems ()
checkTriggerActions world = noProblems
  [ "trigger " ++ show n ++ ", action " ++ show k ++ ": " ++ complaint
  | (n, t) <- zip [0 :: Int ..] (triggers world)
  , (k, a) <- zip [0 :: Int ..] (triggerActions t)
  , complaint <- wrongWith a
  ]
  where
    inactiveMonsters = [mName m | m <- monsters world, mInactive m]
    dormantItems = [iName i | i <- items world, iInactive i]

    wrongWith action = case action of
      SpawnItem name pos
        | not (any (\i -> iName i == name && iPosition i == pos) (items world)) ->
            [ "spawns " ++ show name ++ " at " ++ showPos pos
              ++ ", where the level places no such item" ]
      SpawnMonster name pos
        | name `notElem` inactiveMonsters ->
            [ "calls up " ++ show name
              ++ ", and the level has no inactive monster of that name to call" ]
        | not (standable world pos) ->
            ["would put " ++ show name ++ " inside a wall at " ++ showPos pos]
      AddToInventory name
        | name `notElem` dormantItems ->
            [ "hands over " ++ show name
              ++ ", and the level has no inactive item of that name to hand over" ]
      UnlockDoor pos
        | not (any ((== pos) . dePosition) (doors world)) ->
            ["unlocks the door at " ++ showPos pos ++ ", where there is no door"]
      TransportPlayer pos
        | not (standable world pos) ->
            ["would drop the player inside a wall at " ++ showPos pos]
      ShiftTile pos _
        | isNothing (gridLookup (mapGrid world) pos) ->
            ["changes the tile at " ++ showPos pos ++ ", which is off the map"]
      _ -> []

-- The stairs between two levels have to be at the same place.
--
-- Going up or down leaves the player where they are and only changes which
-- level that is, so stairs that do not line up drop them into a wall.
-- One name, one item.
--
-- Two items that share a name but differ in what they do are two items
-- wearing one label, and the player is the one who finds out: the inventory
-- stacks by name, category and effect value, so a "Health Potion" that heals
-- 70 sits in its own row next to the one that heals 60, looking for all the
-- world like a display fault. Doses are the exception -- they are what
-- stacking adds up, so the same potion may be found in twos and threes.
checkItemsAgree :: [World] -> Either Problems ()
checkItemsAgree worlds = noProblems
  [ "item " ++ show name ++ " is defined more than one way: "
    ++ intercalate "; " (map describe (nub shapes))
  | (name, shapes) <- Map.toList byName
  , length (nub shapes) > 1
  ]
  where
    byName = Map.fromListWith (++)
      [ (iName i, [(iCategory i, iEffectValue i, iEffect i, iValue i)])
      | world <- worlds, i <- items world
      ]
    describe (cat, effectValue, effect, worth) =
      show cat ++ ", effect value " ++ show effectValue
        ++ maybe "" (\e -> ", " ++ show e) effect
        ++ ", worth " ++ show worth

-- A shaft is a tile; the rope that works it is an item, and the two need not
-- be anywhere near each other. A dungeon with shafts and nothing to climb
-- them with has drawn a way out that never opens.
--
-- The check is of the dungeon and not of each floor, because a shaft is
-- climbed on the way back as readily as on the way down: the one on the
-- first floor is the way out, and is reached by coming up to it carrying a
-- rope found below. "A rope at or above this floor" would call that broken,
-- and it is the whole point of it.
checkShaftsClimbable :: [World] -> Either Problems ()
checkShaftsClimbable worlds
  | null shafts || not (null ropes) = noProblems []
  | otherwise = noProblems
      [ "level " ++ show ix ++ ": the shaft at " ++ showPos pos
        ++ " can never be climbed; no item in the dungeon has the"
        ++ " \"Escape\" effect"
      | (ix, pos) <- shafts
      ]
  where
    ropes = [i | world <- worlds, i <- items world, iEffect i == Just Escape]
    shafts =
      [ (ix :: Int, V2 x y)
      | (ix, world) <- zip [0 ..] worlds
      , (y, row) <- zip [0 ..] (mapGrid world)
      , (x, tile) <- zip [0 ..] row
      , tile == Shaft
      ]

-- A monster that outranges the player's eyes is a wound from nowhere: the
-- player is hit, told what hit them, and cannot see it or reach it. Whether
-- that is unfair is a judgement, but it is certainly not what anybody meant
-- to author, so it is refused rather than shipped.
checkShootersAreVisible :: [World] -> Either Problems ()
checkShootersAreVisible worlds = noProblems
  [ "level " ++ show ix ++ ": " ++ show (mName m) ++ " strikes from "
    ++ show reach ++ " away, " ++ complaint
  | (ix, world) <- zip [0 :: Int ..] worlds
  , m <- monsters world
  , Just reach <- [mRange m]
  , complaint <- reasons reach
  ]
  where
    reasons reach
      | reach < 1 = ["which is closer than arm's length"]
      | reach > defaultFogRadius =
          ["further than the player can see (" ++ show defaultFogRadius ++ ")"]
      | otherwise = []

checkStairsMeet :: [World] -> Either Problems ()
checkStairsMeet worlds =
  noProblems (concat (zipWith3 between [0 :: Int ..] worlds (drop 1 worlds)))
  where
    between ix above below =
      case (tilesOf DownStair above, tilesOf UpStair below) of
        ([], _) -> ["level " ++ show ix ++ " has no \">\" stairs down, but a level follows it"]
        (_, []) -> ["level " ++ show (ix + 1) ++ " has no \"<\" stairs up"]
        (down : _, up : _)
          | down == up -> []
          | otherwise ->
              [ "level " ++ show ix ++ " goes down at " ++ showPos down
                ++ " but level " ++ show (ix + 1) ++ " comes up at " ++ showPos up
              ]

-- An item a trigger asks the player to be carrying has to exist by then.
--
-- Anywhere at or above that level counts, because the player carries what
-- they pick up: a rope found on the second floor is what opens the way out
-- of the sixth.
checkTriggerItems :: [World] -> Either Problems ()
checkTriggerItems worlds = noProblems (concat (zipWith missing [0 :: Int ..] worlds))
  where
    carriedBy ix = Set.fromList [iName i | world <- take (ix + 1) worlds, i <- items world]
    missing ix world =
      [ "level " ++ show ix ++ ": trigger " ++ show n ++ " needs item(s) "
        ++ intercalate ", " (map show absent)
        ++ ", which nothing down to here provides"
      | (n, t) <- zip [0 :: Int ..] (triggers world)
      , let asked = itemsAskedFor (triggerCondition t)
                      ++ [name | ConsumeItem name <- triggerActions t]
      , let absent = filter (`Set.notMember` carriedBy ix) asked
      , not (null absent)
      ]
    itemsAskedFor condition = case condition of
      HasItem name -> [name]
      AtPositionWithItems _ required -> required
      _ -> []

-- A locked door needs a key the player can already have, or a trigger that
-- opens it. Keys carry between levels, so anything found on the way down
-- counts.
checkDoorsOpenable :: [World] -> Either Problems ()
checkDoorsOpenable worlds = noProblems (concat (zipWith shut [0 :: Int ..] worlds))
  where
    keysDownTo ix =
      Set.fromList [iName i | world <- take (ix + 1) worlds, i <- items world, iCategory i == Key]
    openedBy world =
      Set.fromList [pos | t <- triggers world, UnlockDoor pos <- triggerActions t]
    shut ix world =
      [ "level " ++ show ix ++ ": the door at " ++ showPos (dePosition d)
        ++ " needs " ++ show (deKeyName d)
        ++ ", which no level down to here provides, and no trigger opens it"
      | d <- doors world
      , deLocked d
      , not (Set.member (deKeyName d) (keysDownTo ix))
      , not (Set.member (dePosition d) (openedBy world))
      ]

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

-- | The next rung of the experience table, and how much more experience is
-- wanted to reach it. Nothing once there is no rung above.
--
-- The rung is found the way levelUp finds it -- the first entry above the
-- level the player is on -- so the figure shown is the one that will
-- actually set the level off, whatever order the table is written in.
nextXPLevel :: GameState -> Maybe (XPLevel, Int)
nextXPLevel state = do
  rung <- find ((> playerXPLevel (player state)) . xpLevel) (xpLevels state)
  pure (rung, max 0 (xpThreshold rung - xp (player state)))

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
charToTile '^' = Shaft
charToTile _   = Floor -- Default to Floor for unknown characters.

tileToChar :: Tile -> Char
tileToChar Wall      = '#'
tileToChar Floor     = '.'
tileToChar Door      = '+'
tileToChar UpStair   = '<'
tileToChar DownStair = '>'
tileToChar Start     = 'S'
tileToChar Shaft     = '^'

-- Find the tile the player starts on, if the map marks one
findStartingPosition :: World -> Maybe (V2 Int)
findStartingPosition wrld =
  listToMaybe
    [ V2 x y
    | (y, row) <- zip [0 ..] (mapGrid wrld)
    , (x, tile) <- zip [0 ..] row
    , tile == Start
    ]
