-- src/Game/Autoplay.hs
--
-- A competent player, played by the machine.
--
-- The dungeon is drawn by hand, so nothing about it guarantees it can be
-- finished: a key may sit behind the door it opens, a floor may hold more
-- monsters than the healing on it can pay for. This plays the game through
-- the same entry points the keyboard uses, so if it gets out alive then a
-- person can. It is deliberately unclever. It heals when hurt, fights what
-- is in the way, picks up what it passes, unlocks what it can and walks
-- down. A dungeon that defeats it is not necessarily unfair, but a dungeon
-- it beats is certainly beatable.
module Game.Autoplay
  ( Outcome (..)
  , Report (..)
  , autoplay
  , stepOnce
  ) where

import Data.List (find, sortOn)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Game.GridUtils (gridLookup, keyedInventory, orthogonal)
import Game.Logic
import Game.State
import Game.Types
import Linear.V2 (V2 (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Outcome
  = Escaped -- ^ Reached a way out and took it
  | Died    -- ^ Killed on the way
  | Stuck   -- ^ Ran out of ideas, or out of turns
  deriving (Eq, Show)

data Report = Report
  { outcome        :: Outcome
  , turnsTaken     :: Int
  , deepestReached :: Int -- ^ Counting from 1, as the player sees it
  , treasure       :: Int
  , finalXp        :: Int
  , finalHealth    :: Int
  , lowestHealth   :: Int -- ^ As a percentage of the maximum, at its worst
  , transcript     :: [String] -- ^ Newest first, as the game keeps it
  }
  deriving (Show)

-- | One turn: the keys it would press, and where that leaves the game.
-- Nothing when it has run out of ideas. Exposed for working out why a
-- dungeon defeats it.
stepOnce :: GameState -> Maybe ([Char], GameState)
stepOnce state = do
  keys <- decide state
  pure (keys, foldl (flip press) state keys)

-- | Play until the game ends or the turn limit runs out.
autoplay :: Int -> GameState -> Report
autoplay limit = go 0 100
  where
    go turns low state
      | gameWon state = report Escaped turns low' state
      | gameOver state = report Died turns low' state
      | turns >= limit = report Stuck turns low' state
      | otherwise = case decide state of
          Nothing -> report Stuck turns low' state
          Just keys -> go (turns + 1) low' (foldl (flip press) state keys)
      where
        low' = min low (healthPct state)

    healthPct state = health (player state) * 100 `div` max 1 (maxHealth state)

    report result turns low state = Report
      { outcome = result
      , turnsTaken = turns
      , deepestReached = deepestLevel state + 1
      , treasure = treasureCarried state
      , finalXp = xp (player state)
      , finalHealth = health (player state)
      , lowestHealth = low
      , transcript = message state
      }

-- One keystroke, dispatched the way the user interface dispatches it.
press :: Char -> GameState -> GameState
press c state
  | commandMode state = handleCommandInputInternal (Just c) False state state
  | otherwise = handleMovementInternal (Just c) state

-- What to do this turn, as a keystroke or two. Nothing means out of ideas.
decide :: GameState -> Maybe [Char]
decide state =
  firstJust
    [ drinkIfHurt state
    , equipIfBetter state
    , unlockIfPossible state
    , grabUnderfoot state
    , dropDeadWeight state
    , headFor (wants state) state
    , headFor (prey state) state
    , descendIfHere state
    , headFor (Set.fromList (stairsDown state)) state
      -- Leaving is the last resort, so that a run goes as deep as the
      -- dungeon allows before it takes the way out.
    , headFor (waysOut state) state
    ]
  where
    firstJust = fromMaybe Nothing . find isJust

-- Healing is worth spending before dying with it in the pack.
drinkIfHurt :: GameState -> Maybe [Char]
drinkIfHurt state
  | health (player state) * 100 >= maxHealth state * 45 = Nothing
  | otherwise = case find (usable . snd) keyed of
      Nothing -> Nothing
      Just (key, _) -> Just ['u', key]
  where
    keyed = keyedInventory (inventory (player state))
              (equippedWeapon (player state)) (equippedArmor (player state))
    usable i = iCategory i == Healing && iUses i /= Just 0

-- Wear the best thing carried. A player who leaves a better blade in the
-- pack is not the player this is meant to stand in for.
equipIfBetter :: GameState -> Maybe [Char]
equipIfBetter state = firstJust [upgrade Weapon equippedWeapon, upgrade Armor equippedArmor]
  where
    firstJust = fromMaybe Nothing . find isJust
    plyr = player state
    keyed = keyedInventory (inventory plyr) (equippedWeapon plyr) (equippedArmor plyr)
    upgrade cat worn =
      let best = sortOn (negate . iEffectValue . snd) [p | p <- keyed, iCategory (snd p) == cat]
          current = maybe 0 iEffectValue (worn plyr)
       in case best of
            ((key, i) : _) | iEffectValue i > current -> Just ['u', key]
            _ -> Nothing

-- Would this be picked up if it were underfoot?
--
-- One rule, used both to decide where to walk and whether to bend down. Two
-- rules would let the player walk to something it then refuses to take, and
-- then set off for the next one, back and forth for ever.
canTake :: GameState -> Item -> Bool
canTake state i = better && room
  where
    plyr = player state
    carried = length (inventory plyr)
    better = case iCategory i of
      -- Gear no better than what is worn only fills the pack up.
      Weapon -> iEffectValue i > maybe 0 iEffectValue (equippedWeapon plyr)
      Armor -> iEffectValue i > maybe 0 iEffectValue (equippedArmor plyr)
      _ -> True
    -- With room to spare, take anything. On the last slot, take a key or a
    -- draught, or something worth more than the poorest thing already
    -- carried. That last clause is also what stops a shed item being picked
    -- straight back up: it is, by definition, not worth more than what was
    -- kept in preference to it.
    room
      | carried < maxInventorySize - 1 = True
      | carried >= maxInventorySize = False
      | iCategory i `elem` [Key, Healing] = True
      | otherwise = iValue i > worstSpareValue state

-- The poorest thing in the pack that could be put down, if any.
worstSpareValue :: GameState -> Int
worstSpareValue state = case map iValue (spares state) of
  [] -> maxBound
  values -> minimum values

-- What could be put down without regret: not a key, not a draught, not worn,
-- and not something a way out asks to be carried.
spares :: GameState -> [Item]
spares state = filter keepable (inventory plyr)
  where
    plyr = player state
    wayOutNeeds =
      concat [ needed | world <- levels state, t <- triggers world
             , SetGameWon `elem` triggerActions t
             , AtPositionWithItems _ needed <- [triggerCondition t] ]
    keepable i =
      iCategory i `notElem` [Key, Healing]
        && iName i `notElem` wayOutNeeds
        && Just i /= equippedWeapon plyr
        && Just i /= equippedArmor plyr

-- Shed the poorest thing, but only with a reason: the pack is full and
-- something better is lying on this floor. Without the reason the pack
-- would be emptied a piece at a time for nothing.
dropDeadWeight :: GameState -> Maybe [Char]
dropDeadWeight state
  | length (inventory plyr) < maxInventorySize = Nothing
  | not needSomething = Nothing
  | any (\i -> iPosition i == position plyr && not (iInactive i)) (items (currentWorld state)) =
      Nothing -- nothing can be put down on a tile that already holds something
  | otherwise = case sortOn (iValue . snd) [p | p <- keyed, snd p `elem` spares state] of
      ((key, _) : _) -> Just ['x', key]
      [] -> Nothing
  where
    plyr = player state
    keyed = keyedInventory (inventory plyr) (equippedWeapon plyr) (equippedArmor plyr)
    needSomething = any worthMakingRoomFor (items (currentWorld state))
    worthMakingRoomFor i =
      not (iInactive i)
        && ( iCategory i `elem` [Key, Healing]
               || iValue i > worstSpareValue state
           )

-- A locked door next to us, and the key for it in the pack.
unlockIfPossible :: GameState -> Maybe [Char]
unlockIfPossible state = case mapMaybe pairing (doors (currentWorld state)) of
  [] -> Nothing
  ((key, _) : _) -> Just ['u', key]
  where
    here = position (player state)
    keyed = keyedInventory (inventory (player state))
              (equippedWeapon (player state)) (equippedArmor (player state))
    pairing door
      | not (deLocked door) = Nothing
      | not (isAdjacent here (dePosition door)) = Nothing
      | otherwise =
          fmap (\(k, i) -> (k, i))
            (find (\(_, i) -> iCategory i == Key && iName i == deKeyName door) keyed)

-- Anything worth taking on this very tile.
grabUnderfoot :: GameState -> Maybe [Char]
grabUnderfoot state
  | any here (items (currentWorld state)) = Just ['g']
  | otherwise = Nothing
  where
    here i =
      iPosition i == position (player state) && not (iInactive i) && canTake state i

descendIfHere :: GameState -> Maybe [Char]
descendIfHere state
  | gridLookup (mapGrid (currentWorld state)) (position (player state)) == Just DownStair =
      Just ['>']
  | otherwise = Nothing

stairsDown :: GameState -> [V2 Int]
stairsDown state =
  [ V2 x y
  | (y, row) <- zip [0 ..] (mapGrid (currentWorld state))
  , (x, tile) <- zip [0 ..] row
  , tile == DownStair
  ]

-- Everything on this floor still worth walking to. Keys and healing first,
-- since those are what a run fails for want of.
wants :: GameState -> Set.Set (V2 Int)
wants state = Set.fromList (map iPosition (sortOn priority loose))
  where
    loose = [i | i <- items (currentWorld state), not (iInactive i), canTake state i]
    priority i = case iCategory i of
      Key -> 0 :: Int
      Healing -> 1
      _ -> 2

-- Everything still breathing on this floor. A player who means to finish
-- the dungeon clears it, both for the experience and because a trigger may
-- be waiting on something dying.
prey :: GameState -> Set.Set (V2 Int)
prey state =
  Set.fromList [mPosition m | m <- monsters (currentWorld state), not (mInactive m)]

-- Somewhere standing would end the run, and whose price we can already pay.
--
-- Read straight off the triggers, so a way out is wherever the dungeon says
-- it is rather than somewhere this module has been told about.
waysOut :: GameState -> Set.Set (V2 Int)
waysOut state = Set.fromList (mapMaybe exit (triggers (currentWorld state)))
  where
    held = map iName (inventory (player state))
    wins t = SetGameWon `elem` triggerActions t
    exit t
      | not (wins t) = Nothing
      | otherwise = case triggerCondition t of
          AtPosition pos -> Just pos
          AtPositionWithItems pos needed
            | all (`elem` held) needed -> Just pos
          _ -> Nothing

-- Walk one step along the shortest way to the nearest of these.
headFor :: Set.Set (V2 Int) -> GameState -> Maybe [Char]
headFor targets state
  | Set.null targets = Nothing
  | otherwise = do
      step <- nextStep state targets
      keyFor (step - position (player state))
  where
    keyFor delta = case delta of
      V2 0 (-1) -> Just ['w']
      V2 0 1 -> Just ['s']
      V2 (-1) 0 -> Just ['a']
      V2 1 0 -> Just ['d']
      _ -> Nothing

-- Breadth-first, out from the player. Doors we can open count as passable,
-- and so do monsters: walking into one is how you fight it.
nextStep :: GameState -> Set.Set (V2 Int) -> Maybe (V2 Int)
nextStep state targets = search (Map.singleton here here) [here]
  where
    world = currentWorld state
    here = position (player state)
    haveKey name =
      any (\i -> iCategory i == Key && iName i == name) (inventory (player state))
    passable pos =
      gridLookup (mapGrid world) pos `notElem` [Nothing, Just Wall]
        && case find ((== pos) . dePosition) (doors world) of
             Just door -> not (deLocked door) || haveKey (deKeyName door)
             Nothing -> True

    search _ [] = Nothing
    search cameFrom (pos : rest)
      | pos /= here && Set.member pos targets = Just (firstStepTo cameFrom pos)
      | otherwise =
          let found = [n | n <- orthogonal pos, passable n, not (Map.member n cameFrom)]
              cameFrom' = foldr (`Map.insert` pos) cameFrom found
           in search cameFrom' (rest ++ found)

    firstStepTo cameFrom pos =
      let parent = Map.findWithDefault here pos cameFrom
       in if parent == here then pos else firstStepTo cameFrom parent
