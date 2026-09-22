-- src/Game/Logic.hs
module Game.Logic where

import Game.State
  ( defaultMonsterRadius, defaultFogRadius, maxInventorySize
  , updateVisibility, evalTriggerCondition, visibleMonsters
  , currentWorld, setCurrentWorld, withCurrentWorld, replaceLevel, maxLogMessages, maxHealth, npcMoveInterval, nextHelpPage, withRandom, initializeGrid, whatItDoes, seesFrom
  )
import Game.GridUtils (updateTile, gridLookup, orthogonal, keyedInventory)
import Game.Types
import Linear.V2 (V2(..))
import Data.List (find, partition, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, listToMaybe)
import System.Random (StdGen, uniformR)

handleMovementInternal :: Maybe Char -> GameState -> GameState
-- The scoreboard is a sheet of paper held up in front of the game. Any key
-- puts it down again, and putting it down is not a turn.
handleMovementInternal _ state | showScores state = state {showScores = False}
handleMovementInternal _ state | showLog state = state {showLog = False}
handleMovementInternal key state =
  case aimingState state of
    -- Delegate to the aiming logic, which hands back a state transformer
    Just _ -> let aim = handleCommandInputInternal key False state in aim state
    Nothing ->
      let isGameOverOrWon = gameOver state || gameWon state
          acted = case key of
            _ | isGameOverOrWon -> state -- Prevent movement if game is won/over (except '?' and ':')
            Just c | c == 'w' || c == 'k' -> movePlayer North state
            Just c | c == 's' || c == 'j' -> movePlayer South state
            Just c | c == 'a' || c == 'h' -> movePlayer West state
            Just c | c == 'd' || c == 'l' -> movePlayer East state
            Just '<' -> goUp state
            Just '>' -> goDown state
            Just 'g' -> pickUpItem state
            Just 'u' -> promptUseItem state
            Just 'x' -> promptDropItem state
            _ -> state
      in case key of
           -- Toggling the legend and opening command mode are not turns, so
           -- they must not move monsters or advance the NPC clock.
           Just '?' -> state { legendPage = nextHelpPage (legendPage state) }
           Just ':' -> state { commandMode = True, commandBuffer = ":" }
           _ | isGameOverOrWon -> acted
           _ -> processTurn acted

-- One turn: the clock ticks, then monsters move and attack, triggers fire,
-- and the NPCs take a step every npcMoveInterval turns.
processTurn :: GameState -> GameState
processTurn state =
  let ticked = regenerate state
        { keyPressCount = (keyPressCount state + 1) `mod` npcMoveInterval
        , turnCount = turnCount state + 1
        , hiddenTurns = max 0 (hiddenTurns state - 1)
        }
      state' = moveMonsters ticked
      state'' = monstersAttack state'
      state''' = processTriggers state''
      state'''' = if keyPressCount state''' == 0 then moveNPCs state''' else state'''
  in state'''' { message = take maxLogMessages (message state'''') }

-- Heal a little each turn, for as long as the charm is carried
regenerate :: GameState -> GameState
regenerate state = case carrying Regenerate state of
  Nothing -> state
  Just charm
    | health (player state) >= maxHealth state -> state
    | otherwise ->
        let healed = min (maxHealth state) (health (player state) + iEffectValue charm)
         in state {player = (player state) {health = healed}}

-- Go up stairs
goUp :: GameState -> GameState
goUp state =
  case gridLookup (mapGrid (currentWorld state)) state.player.position of
    Just UpStair
      | currentLevel state > 0 ->
          let newLevel = currentLevel state - 1
              updatedWorld = updateVisibility (player state) defaultFogRadius (levels state !! newLevel)
           in state { currentLevel = newLevel
                    , levels = replaceLevel state newLevel updatedWorld
                    , message = "You ascend the stairs." : message state }
      | otherwise -> state { message = "You are already on the top level." : message state }
    _ -> state { message = "No stairs to go up here!" : message state }

-- Go down stairs
goDown :: GameState -> GameState
goDown state =
  case gridLookup (mapGrid (currentWorld state)) state.player.position of
    Just DownStair
      | currentLevel state < length (levels state) - 1 ->
          let newLevel = currentLevel state + 1
              updatedWorld = updateVisibility (player state) defaultFogRadius (levels state !! newLevel)
           in state { currentLevel = newLevel
                    , deepestLevel = max (deepestLevel state) newLevel
                    , levels = replaceLevel state newLevel updatedWorld
                    , message = "You descend the stairs." : message state }
      | otherwise -> state { message = "You are already on the bottom level." : message state }
    _ -> state { message = "No stairs to go down here!" : message state }

-- Picking something up is the moment its description is worth reading: it
-- is the one time the player is certainly looking at that item and nothing
-- else, and the inventory has no room for a sentence.
pickedUp :: Item -> String
pickedUp itm =
  "You picked up: " ++ iName itm ++ " - " ++ whatItDoes itm
    ++ (if null (iDescription itm) then "" else ". " ++ iDescription itm)

pickUpItem :: GameState -> GameState
pickUpItem state =
  let world = currentWorld state
      playerPos = state.player.position
      inventorySize = length (state.player.inventory)
      (itemsOnTile, remainingItems) =
          partition (\item -> iPosition item == playerPos && not (iInactive item)) (items world)
  in case itemsOnTile of
       [] -> state { message = "There is nothing to pick up here." : message state }
       (item:_) ->
         let existingStackableItem = find
               (\invItem -> iName invItem == iName item
                        && iCategory invItem == iCategory item
                        && iEffectValue invItem == iEffectValue item
                        && isJust (iUses invItem))
               (state.player.inventory)

             (invFull, invMsgs, updatedInventory) = case (existingStackableItem, iUses item) of
               -- Stacking adds the doses and the worth both. Adding only the
               -- doses meant a second flask scored nothing, so two potions
               -- that stacked were worth less carried out than two that did
               -- not -- which made a pair of mismatched items look like a
               -- bonus and a matched pair look like a loss.
               (Just invItem, Just uses) ->
                 (False, [pickedUp item], map (\i -> if i == invItem
                            then i { iUses = fmap (+ uses) (iUses i)
                                   , iValue = iValue i + iValue item }
                            else i)
                     (state.player.inventory))
               _ -> if inventorySize >= maxInventorySize
                    then (True, ["Your inventory is full! Drop an item before picking up more."], state.player.inventory)
                    else (False, [pickedUp item], item : state.player.inventory)  -- Add as a new item if not stackable

             updatedPlayer = (player state) { inventory = updatedInventory }
             updatedWorld = if invFull then world else world { items = remainingItems }
         in setCurrentWorld updatedWorld $ state
              { player = updatedPlayer
              , message = invMsgs ++ message state
              }

-- Player has requested to use an item, prompt which item to use
promptUseItem :: GameState -> GameState
promptUseItem state =
  let inv = state.player.inventory
  in if null inv
       then state { message = "Your inventory is empty." : message state }
       else state { message = "Press a key to use an item." : message state
                  , commandMode = True
                  , inventoryMode = Just UseMode }

-- Player has requested to drop an item, prompt which item to drop
promptDropItem :: GameState -> GameState
promptDropItem state =
  let inv = state.player.inventory
  in if null inv
       then state { message = "Your inventory is empty." : message state }
       else state { message = "Press a key to drop an item." : message state
                  , commandMode = True
                  , inventoryMode = Just DropMode }

-- Helper to reduce item uses or remove it if depleted
reduceUses :: Item -> [Item] -> [Item]
reduceUses itm inv =
  filter (\item -> iUses item /= Just 0) $
  map (updateUses itm) inv

-- Handle the usage of an item from the inventory
useItem :: Item -> GameState -> GameState
useItem itm state =
  let plyr = player state
      doorToUnlock = find (isAdjacent (position plyr) . dePosition)
                          (filter deLocked (doors (currentWorld state)))
      recalculateEffectiveStats p = p
        { attack = baseAttack p + maybe 0 iEffectValue (equippedWeapon p)
        , resistance = baseResistance p + maybe 0 iEffectValue (equippedArmor p) }

      updatedState = case iCategory itm of
        Healing ->
          let playerCurrentMaxHealth = maxHealth state
              healedHealth = min playerCurrentMaxHealth (health plyr + iEffectValue itm)
           in state { player = plyr { health = healedHealth
                                        , inventory = reduceUses itm (inventory plyr) }
                    , message = ("You used " ++ iName itm ++ " and recovered "
                                     ++ show (iEffectValue itm) ++ " HP.") : message state }
        Key ->
          case doorToUnlock of
            Just door | iName itm == deKeyName door ->
              unlockDoor state plyr door itm
            Just _ ->
              state { message = "This key does not fit the lock!" : message state }
            Nothing ->
              state { message = "There is no door nearby to unlock." : message state }
        Weapon ->
          let newPlayer = if Just itm == equippedWeapon plyr
                          then plyr { equippedWeapon = Nothing }
                          else plyr { equippedWeapon = Just itm }
              newPlayerWithNewAttack = recalculateEffectiveStats newPlayer
              msg = if Just itm == equippedWeapon plyr
                    then "You unequipped " ++ iName itm ++ "."
                    else "You equipped " ++ iName itm ++ "."
           in state { player = newPlayerWithNewAttack
                    , message = msg : message state }
        Range ->
          state { aimingState = Just (AimingState itm)
                , commandMode = True
                , message = ("You prepare to use " ++ iName itm ++ ". Press a key to aim.") : message state }
        Armor  ->
          let newPlayer = if Just itm == equippedArmor plyr
                          then plyr { equippedArmor = Nothing }
                          else plyr { equippedArmor = Just itm }
              newPlayerWithNewResistance = recalculateEffectiveStats newPlayer
              msg = if Just itm == equippedArmor plyr
                    then "You unequipped " ++ iName itm ++ "."
                    else "You equipped " ++ iName itm ++ "."
           in state { player = newPlayerWithNewResistance
                    , message = msg : message state }
        Special -> useSpecial itm state
   in updatedState { inventoryMode = Nothing }

-- Use a Special item, doing whatever its effect says.
--
-- Effects that fire once spend the item; the ones that work while it is
-- carried leave it alone and say so, so that using one is not a way to
-- throw it away by accident.
useSpecial :: Item -> GameState -> GameState
useSpecial itm state = case iEffect itm of
  Nothing -> say (iName itm ++ " does nothing at all.") state
  Just effect ->
    let acted = apply effect
     in if spentOnUse effect then spend acted else acted
  where
    plyr = player state
    value = iEffectValue itm
    say msg s = s {message = msg : message s}
    spend s = s {player = (player s) {inventory = filter (/= itm) (inventory (player s))}}

    -- Equipment bonuses sit on top of the base figures, so a permanent gain
    -- has to be folded back into the effective ones.
    recalculated p = p
      { attack = baseAttack p + maybe 0 iEffectValue (equippedWeapon p)
      , resistance = baseResistance p + maybe 0 iEffectValue (equippedArmor p)
      }

    apply Keepsake =
      say (iName itm ++ " is not something you can use.") state
    apply Empower =
      say ("You feel stronger. Attack is up by " ++ show value ++ ".")
        state {player = recalculated plyr {baseAttack = baseAttack plyr + value}}
    apply Fortify =
      say ("You feel tougher. Resistance is up by " ++ show value ++ ".")
        state {player = recalculated plyr {baseResistance = baseResistance plyr + value}}
    apply Reveal =
      say "The layout of this floor comes to you all at once." $
        withCurrentWorld
          (\w -> w {discovered = initializeGrid True (mapRows w) (mapCols w)})
          state
    apply Blink =
      case blinkTargets (currentWorld state) (position plyr) of
        [] -> say "There is nowhere to go." state
        spots ->
          let (ix, moved) = withRandom (uniformR (0, length spots - 1)) state
              destination = spots !! ix
              relocated = moved {player = (player moved) {position = destination}}
           in say "The floor lurches, and you are somewhere else." $
                withCurrentWorld (updateVisibility (player relocated) defaultFogRadius) relocated
    apply Firestorm = firestorm value state
    apply Regenerate =
      say (iName itm ++ " works away quietly while you carry it.") state
    apply Lifesteal =
      say (iName itm ++ " drinks from the wounds you deal while you carry it.") state
    apply Revive =
      say (iName itm ++ " will catch you once, while you carry it.") state
    apply Vanish =
      say ("Nothing can see you for " ++ show value ++ " turns.")
        state {hiddenTurns = hiddenTurns state + value}
    apply Escape = climbOut itm state

-- Throw a rope up a shaft and climb it.
--
-- A shaft is one floor's worth of daylight, not the way out: it puts the
-- player on the floor above, beside the stairs they came down, which is the
-- only spot up there that is certain to exist and be walkable. From the top
-- floor there is no floor above, and climbing is leaving -- that is the run
-- over, scored on what came up with them.
--
-- It is deliberately something the player does rather than something that
-- happens to them. Standing on a shaft says so and costs nothing; only using
-- the rope commits.
climbOut :: Item -> GameState -> GameState
climbOut rope state
  | gridLookup (mapGrid (currentWorld state)) state.player.position /= Just Shaft =
      say "There is nothing overhead to throw this over." state
  | currentLevel state <= 0 =
      say "You climb into the open air, and the dungeon is behind you."
        (spent state) {gameWon = True}
  | otherwise =
      case landing of
        Nothing -> say "The rope finds nothing to catch on." state
        Just spot ->
          let above = currentLevel state - 1
              climbed = (spent state)
                { currentLevel = above
                , player = (climber state) {position = spot}
                }
           in say "You throw the rope, climb, and come out by the stairs."
                climbed
                  { levels = replaceLevel climbed above
                      (updateVisibility (player climbed) defaultFogRadius
                         (levels state !! above))
                  }
  where
    say msg s = s {message = msg : message s}
    climber s = (player s) {inventory = filter (/= rope) (inventory (player s))}
    spent s = s {player = climber s}
    landing = do
      above <-
        if currentLevel state > 0
          then Just (levels state !! (currentLevel state - 1))
          else Nothing
      listToMaybe
        [ V2 x y
        | y <- [0 .. mapRows above - 1]
        , x <- [0 .. mapCols above - 1]
        , gridLookup (mapGrid above) (V2 x y) == Just DownStair
        ]

-- Floor tiles the player could be dropped on: anywhere they could walk to
-- from where they stand, and not on top of something else.
--
-- Reachable from where they stand, and not merely somewhere on the level: a
-- blink that can cross a locked door is a key. It put the player inside the
-- vault on the bottom floor, where the door wants a sigil carried by the
-- thing standing outside it -- the run ended there, with no way back
-- through and nothing left to do.
blinkTargets :: World -> V2 Int -> [V2 Int]
blinkTargets world from =
  [ pos
  | pos <- Set.toList (walkableFrom world from)
  , gridLookup (mapGrid world) pos == Just Floor
  , not (any ((== pos) . mPosition) (filter (not . mInactive) (monsters world)))
  , not (any ((== pos) . npcPosition) (npcs world))
  ]

-- Every tile that can be walked to from here, a locked door being a wall.
walkableFrom :: World -> V2 Int -> Set.Set (V2 Int)
walkableFrom world from = go (Set.singleton from) [from]
  where
    go seen [] = seen
    go seen (pos : rest) =
      let next = [n | n <- orthogonal pos, isWalkable world n, not (Set.member n seen)]
       in go (foldr Set.insert seen next) (rest ++ next)

-- Hurt every monster the player can see.
firestorm :: Int -> GameState -> GameState
firestorm power state =
  let world = currentWorld state
      targets = map snd (visibleMonsters world)
      (hits, rolled) = foldl' roll ([], state) targets
      roll (done, s) target =
        let (damage, s') = withRandom (rollDamage power) s
         in ((target, damage) : done, s')
      hurt m = case lookup m hits of
        Just damage -> m {mHealth = mHealth m - damage}
        Nothing -> m
      struck = map hurt (monsters world)
      (felled, standing) = partition (\m -> not (mInactive m) && mHealth m <= 0) struck
      updatedWorld = world
        { monsters = standing
        , corpses = foldr (addCorpse . mPosition) (corpses world) felled
        }
      gained = sum (map mXP felled)
      (grown, levelUpMessages) =
        levelUp ((player rolled) {xp = xp (player rolled) + gained}) (xpLevels rolled)
      told
        | null targets = ["Fire washes over nothing in particular."]
        | otherwise =
            ("Fire washes over " ++ show (length targets) ++ " of them!")
              : [ "You defeated " ++ mName m ++ "!" | m <- felled ]
              ++ [ "You gained " ++ show gained ++ " XP!" | gained > 0 ]
      noted = foldr recordDefeat rolled felled
   in setCurrentWorld updatedWorld
        noted {player = grown, message = reverse told ++ levelUpMessages ++ message rolled}

-- The first item in the pack with this effect, if there is one
carrying :: ItemEffect -> GameState -> Maybe Item
carrying effect state = find ((== Just effect) . iEffect) (inventory (player state))

-- Can the monsters see the player at all?
playerIsHidden :: GameState -> Bool
playerIsHidden state = hiddenTurns state > 0

-- Roll the damage an attack of this strength does.
--
-- Within a quarter either side of the attacker's strength, so that a fight
-- is not arithmetic with a knowable answer. The average is the strength
-- itself, which leaves the balance of the game where it already was. An
-- attack that cannot get through the defender's resistance still does
-- nothing at all rather than rolling a point of damage anyway.
rollDamage :: Int -> StdGen -> (Int, StdGen)
rollDamage base gen
  | base <= 0 = (0, gen)
  | otherwise = uniformR (max 1 (base - spread), base + spread) gen
  where
    spread = max 1 (base `div` 4)

-- Find the active monster standing on a tile.
--
-- Monsters are one to a tile, so a position identifies a target. Looking the
-- target up this way means a caller holding a stale copy of the monster still
-- hits the right one; matching on the value would silently miss once any of
-- its fields had changed. Inactive spawn templates are not targets, even
-- though one can share a tile with a live monster.
activeMonsterAt :: V2 Int -> World -> Maybe Monster
activeMonsterAt pos world =
  find (\m -> not (mInactive m) && mPosition m == pos) (monsters world)

-- Remember that a monster of this name has been beaten, so that a trigger
-- can wait on it. Names rather than positions, because that is what a world
-- file has to refer to.
recordDefeat :: Monster -> GameState -> GameState
recordDefeat monster state
  | mName monster `elem` defeatedMonsters state = state
  | otherwise = state {defeatedMonsters = mName monster : defeatedMonsters state}

-- Record where a monster fell, without duplicating a position
addCorpse :: V2 Int -> [V2 Int] -> [V2 Int]
addCorpse pos poss
  | pos `elem` poss = poss
  | otherwise       = pos : poss

-- Helper function: Check adjacency
isAdjacent :: V2 Int -> V2 Int -> Bool
isAdjacent (V2 x1 y1) (V2 x2 y2) =
  abs (x1 - x2) + abs (y1 - y2) == 1

-- Helper function to actually unlock a door using a specific key
unlockDoor :: GameState -> Player -> DoorEntity -> Item -> GameState
unlockDoor state plyr door key =
  let updatedDoors = map (\d -> if d == door then d { deLocked = False } else d)
                           (doors (currentWorld state))
      updatedWorld = (currentWorld state) { doors = updatedDoors }
      updatedInventory = reduceUses key (inventory plyr) -- Remove or decrement key stack
      updatedPlayer = plyr { inventory = updatedInventory }
  in setCurrentWorld updatedWorld $ state
       { player = updatedPlayer
       , message = ("You used " ++ iName key ++ " to unlock the door!") : message state }

-- Handle the case where the player wants to drop an item from the inventory
dropItem :: Item -> GameState -> GameState
dropItem item state =
  let plyr = player state
      world = currentWorld state
      playerPos = position plyr
      itemsOnTile = filter (\i -> iPosition i == playerPos && not (iInactive i)) (items world)

  in if not (null itemsOnTile)
     then state { message = "You cannot drop an item here, the space is occupied!" : message state }
     else
       let updatedInventory = filter (/= item) (inventory plyr)
           droppedItem = item { iPosition = playerPos, iInactive = False }
           updatedWorld = world { items = droppedItem : items world }
       in setCurrentWorld updatedWorld $ state
            { player = plyr { inventory = updatedInventory }
            , message = ("You dropped: " ++ iName item) : message state
            , inventoryMode = Nothing
            }

-- Range attack handling (getVisibleMonsters, executeRangedAttack, calculateRangedDamage)
getVisibleMonsters :: GameState -> [(Char, Monster)]
getVisibleMonsters state =
  visibleMonsters (currentWorld state)

-- Helper to update item uses
updateUses :: Item -> Item -> Item
updateUses usedItem item
  | item == usedItem = item { iUses = fmap (\n -> n - 1) (iUses item) }
  | otherwise = item

executeRangedAttack :: GameState -> Monster -> Item -> GameState
executeRangedAttack state targetMonster rangedItem =
  case activeMonsterAt (mPosition targetMonster) world of
    Nothing -> state -- No live monster on that tile any more
    Just target ->
      let (damage, rolled) =
            withRandom (rollDamage (calculateRangedDamage (player state) target rangedItem)) state
          monsterDefeated = mHealth target - damage <= 0

          isTarget m = not (mInactive m) && mPosition m == mPosition target
          updatedMonsters =
            if monsterDefeated
            then filter (not . isTarget) (monsters world)
            else map (\m -> if isTarget m then m { mHealth = mHealth m - damage } else m)
                     (monsters world)

          -- Mark the position where the monster was defeated
          updatedCorpses =
            if monsterDefeated
            then addCorpse (mPosition target) (corpses world)
            else corpses world
          updatedWorld = world { monsters = updatedMonsters, corpses = updatedCorpses }

          defeatMessage = if monsterDefeated then "You defeated " ++ mName target ++ "!" else ""
          xpGainMessage = if monsterDefeated then "You gained " ++ show (mXP target) ++ " XP!" else ""
          attackMessage = "You hit " ++ mName target ++ " for " ++ show damage ++ " damage!"
          (updatedPlayer, levelUpMessages) =
            if monsterDefeated
            then levelUp ((player rolled) { xp = rolled.player.xp + mXP target })
                         (xpLevels rolled)
            else (player rolled, [])
          completeMessages = levelUpMessages
            ++ filter (not . null) [defeatMessage, xpGainMessage, attackMessage]
          updatedPlayerWithReducedUsesForItem =
            updatedPlayer { inventory = reduceUses rangedItem (inventory updatedPlayer) }
          noted = if monsterDefeated then recordDefeat target rolled else rolled
       in setCurrentWorld updatedWorld $ noted
            { player = updatedPlayerWithReducedUsesForItem
            , message = completeMessages ++ message rolled }
  where
    world = currentWorld state

calculateRangedDamage :: Player -> Monster -> Item -> Int
calculateRangedDamage plyr mnstr rangedItem =
  let baseDamage = attack plyr
      rangedBonus = iEffectValue rangedItem
      monsterResistance = max 0 (mHealth mnstr `div` 10) -- Example: Monster's resistance based on health
      totalDamage = max 0 (baseDamage + rangedBonus - monsterResistance)
  in totalDamage

handleCommandInputInternal :: Maybe Char -> Bool -> GameState -> (GameState -> GameState)
handleCommandInputInternal key esc state =
  case aimingState state of
    Just (AimingState rangedItem) ->
      case key of
        Just c | Just monster <- lookup c (getVisibleMonsters state) ->
          \s -> exitAimingMode (executeRangedAttack s monster rangedItem)
        _ | esc -> exitAimingMode
        _ -> addMessage "Invalid selection. Press ESC to cancel."
    Nothing ->
      if null (commandBuffer state)
        then case key of
          Just c -> processInventorySelection c
          _ | esc -> exitCommandMode
          _ -> id
        else case key of
          Just '\n' -> markCommandForExecution
          Just '\b' -> removeLastCommandChar
          Just c   -> appendToCommandBuffer c
          Nothing | esc -> exitCommandModeAndClearBuffer
          Nothing -> id
  where
    exitAimingMode s = s { aimingState = Nothing, commandMode = False }
    -- Leaving command mode also abandons any pending item choice; otherwise
    -- the chooser stays open with nothing listening for a key.
    exitCommandMode s = s { commandMode = False, inventoryMode = Nothing }
    exitCommandModeAndClearBuffer s = s { commandMode = False, commandBuffer = "" }
    appendToCommandBuffer c s = s { commandBuffer = commandBuffer s ++ [c] }
    removeLastCommandChar s = s { commandBuffer = initSafe (commandBuffer s) }
    addMessage msg s = s { message = msg : message s }
    markCommandForExecution s = s { commandMode = False, commandToExecute = True }

    processInventorySelection c s =
      let inv = s.player.inventory
          eqpdWeapon = s.player.equippedWeapon
          eqpdArmor = s.player.equippedArmor
      in case lookup c (keyedInventory inv eqpdWeapon eqpdArmor) of
        Just item -> case inventoryMode s of
          Just UseMode  -> exitCommandMode $ useItem item s
          Just DropMode -> exitCommandMode $ dropItem item s
          Nothing -> addMessage "Use/Drop error" s
        Nothing -> addMessage "Invalid selection." s

-- Safe init for empty lists
initSafe :: [a] -> [a]
initSafe [] = []
initSafe xs = init xs

-- Move the player in a direction
movePlayer :: Direction -> GameState -> GameState
movePlayer dir state =
  let playerPos = state.player.position
      world = currentWorld state
      worldMap = mapGrid world
      newPos = case dir of
        North -> playerPos + V2 0 (-1)
        South -> playerPos + V2 0 1
        West  -> playerPos + V2 (-1) 0
        East  -> playerPos + V2 1 0
        _          -> playerPos

      -- Helper to find an active monster at a given position
      monsterAt pos = activeMonsterAt pos world

      -- Helper to find a door at a given position
      doorAt pos = find (\d -> dePosition d == pos) (doors world)

      -- Check if the new position is occupied by an NPC
      npcAt pos = find (\npc -> npcPosition npc == pos) (npcs world)

      -- Helper to check if the player can move to a position
      canMove pos =
        let V2 x y = pos
        in y >= 0 && y < mapRows world &&
           x >= 0 && x < mapCols world &&
           (worldMap !! y !! x) /= Wall

      -- Helper to handle movement
      -- Stepping onto a shaft says what it is and nothing more. Before, the
      -- tile itself ended the run the moment a player carrying a rope walked
      -- over it, which made a way out of something that should be an offer.
      noticed pos s
        | gridLookup worldMap pos == Just Shaft =
            s {message = "Daylight falls through a crack overhead." : message s}
        | otherwise = s
      internalHandleMovement nPos =
        let updatedWorld = updateVisibility (player state) defaultFogRadius world
        in noticed nPos $ setCurrentWorld updatedWorld $
             state { player = (player state) { position = nPos } }
  in case (doorAt newPos, monsterAt newPos, npcAt newPos) of
       (Just door, _, _) | deLocked door -> -- Locked door case
         state { message = "The door in front of you is locked and is blocking your way." : message state }
       (_, Nothing, Nothing) | canMove newPos -> -- No monster or NPC
         internalHandleMovement newPos
       (_, Just monster, _) -> -- Monster
         combat state monster True
       -- Walking into an NPC talks to them and changes places with them.
       --
       -- Talking used to leave both of you standing where you were, which
       -- made an NPC in a one-tile corridor a wall: they step aside only on
       -- their own clock, and an NPC with the player on one side and a
       -- monster on the other has nowhere to step at all. That sealed the
       -- passage for good. Swapping costs nothing and cannot deadlock.
       (_, _, Just npc) | canMove newPos ->
         let swapped = w {npcs = map step (npcs w)}
             w = world
             step q | npcName q == npcName npc = q {npcPosition = playerPos}
                    | otherwise = q
          in setCurrentWorld (updateVisibility (player state) defaultFogRadius swapped)
               state { player = (player state) {position = newPos}
                     , message = (npcName npc ++ " says: " ++ npcMessage npc) : message state
                     , lastInteractedNpc = Just (npcName npc)
                     }
       (_, _, Just npc) ->
         state { message = (npcName npc ++ " says: " ++ npcMessage npc) : message state
               , lastInteractedNpc = Just (npcName npc)
               }
       _ -> state -- Invalid move

-- Player hits a monster and the monster returns the favor
combat :: GameState -> Monster -> Bool -> GameState
combat state mnstr playerGoesFirst =
  case activeMonsterAt (mPosition mnstr) world of
    Nothing -> state -- No live monster on that tile any more
    Just target ->
      let plyr = player state
          (playerDamage, rolledOnce) = withRandom (rollDamage (attack plyr)) state
          (monsterDamage, rolled) =
            withRandom (rollDamage (mAttack target - resistance plyr)) rolledOnce
          monsterDefeated = mHealth target - playerDamage <= 0

          -- A charm that drinks from the wounds you deal gives back a share
          -- of the damage, before the counterblow is taken off again.
          -- The charm's value is the percentage it gives back, so a bigger
          -- number is a better charm.
          drained = case carrying Lifesteal rolled of
            Just charm | playerDamage > 0 ->
              min (maxHealth rolled)
                  (health plyr + (playerDamage * iEffectValue charm) `div` 100)
            _ -> health plyr
          wounded = max 0 (drained - monsterDamage)

          (newHealth, survivingPack, rescueMessage) = catchDeath rolled wounded
          updatedPlayer = plyr { health = newHealth, inventory = survivingPack }

          isTarget m = not (mInactive m) && mPosition m == mPosition target
          updatedMonsters =
            if monsterDefeated
            then filter (not . isTarget) (monsters world)
            else map (\m -> if isTarget m then m { mHealth = mHealth m - playerDamage } else m)
                     (monsters world)

          -- Mark the position where the monster was defeated
          updatedCorpses =
            if monsterDefeated
            then addCorpse (mPosition target) (corpses world)
            else corpses world
          updatedWorld = world { monsters = updatedMonsters, corpses = updatedCorpses }

          isDead = newHealth == 0
          defeatMessage = if monsterDefeated
                          then "You defeated the " ++ mName target ++ " and gained " ++ show (mXP target) ++ " XP!"
                          else ""
          attackMessage = if playerGoesFirst
                          then "You attacked " ++ mName target ++ " for " ++ show playerDamage ++ " damage!"
                          else "The " ++ mName target ++ " attacked you for " ++ show monsterDamage ++ " damage!"
          counterattackMessage = if playerGoesFirst
                                 then "The " ++ mName target ++ " counterattacked you for " ++ show monsterDamage ++ " damage!"
                                 else "You counterattacked " ++ mName target ++ " for " ++ show playerDamage ++ " damage!"
          deadMessage = if isDead then "You have died! Game Over." else ""
          -- Events that did not happen contribute "", which would otherwise
          -- take up one of the few lines the message pane shows.
          combatMessages = rescueMessage ++ filter (not . null)
            [deadMessage, defeatMessage, counterattackMessage, attackMessage]
          updatedPlayerWithXP = if monsterDefeated
                                then updatedPlayer { xp = xp updatedPlayer + mXP target }
                                else updatedPlayer
          (updatedPlayerWithXPAndPossibleNewLevel, levelUpMessages) =
              if isDead
              then (updatedPlayerWithXP, [])
              else levelUp updatedPlayerWithXP (xpLevels rolled)
          completeMessage = levelUpMessages ++ combatMessages ++ message rolled
          noted = if monsterDefeated then recordDefeat target rolled else rolled
       in setCurrentWorld updatedWorld $ noted
            { player = updatedPlayerWithXPAndPossibleNewLevel
            , message = completeMessage
            , gameOver = isDead }
  where
    world = currentWorld state

levelUp :: Player -> [XPLevel] -> (Player, [String])
levelUp plyr lvls =
  let currentXP = xp plyr
      currentXPLevel = playerXPLevel plyr
      nextXPLevel = find (\lvl -> currentXP >= xpThreshold lvl && xpLevel lvl > currentXPLevel) lvls
      calculateEffectiveStats p = p
        { attack = baseAttack p + maybe 0 iEffectValue (equippedWeapon p)
        , resistance = baseResistance p + maybe 0 iEffectValue (equippedArmor p)
        }
  in case nextXPLevel of
       Just lvl ->
         let updatedPlayer = plyr
               { playerXPLevel = xpLevel lvl
               , health = xpHealth lvl
               , baseAttack = xpAttack lvl
               , baseResistance = xpResistance lvl
               }
             recalculatedPlayer = calculateEffectiveStats updatedPlayer
         in (recalculatedPlayer,
             [ "You leveled up to level " ++ show (xpLevel lvl) ++ "!"
             , "Health increased to " ++ show (xpHealth lvl) ++ "."
             , "Base attack increased to " ++ show (xpAttack lvl) ++ "."
             , "Base resistance increased to " ++ show (xpResistance lvl) ++ "."
             ])
       Nothing -> (plyr, [])

-- | Whether a monster is able to shoot the player where it stands.
--
-- It needs a range, the player inside it, and a clear line. The same line
-- the player sees by, so nothing can shoot from behind a wall, and -- since
-- a monster's range is checked against what the player can see -- nothing
-- can shoot out of the dark either.
canShoot :: World -> Monster -> V2 Int -> Bool
canShoot world monster playerPos = case mRange monster of
  Nothing -> False
  Just reach ->
    not (isAdjacent (mPosition monster) playerPos)
      && seesFrom world reach (mPosition monster) playerPos

-- Monsters in tiles adjacent to the player should attack
monstersAttack :: GameState -> GameState
monstersAttack state
  | playerIsHidden state = state -- nothing can find the player to swing at
  | otherwise =
  let world = currentWorld state
      playerPos = state.player.position
      (_, activeMonsters) = partition mInactive (monsters world)
      withinReach = filter (\m ->
          isAdjacent (mPosition m) playerPos || canShoot world m playerPos) activeMonsters
    in foldl' monsterAttackOrWait state withinReach

-- Helper for handling either monster going into combat or monster waiting
monsterAttackOrWait :: GameState -> Monster -> GameState
monsterAttackOrWait state mnstr =
  let world = currentWorld state
      mnstrUpdated = mnstr { mAttackWait = not (mAttackWait mnstr) }
      updatedMonsters = replaceFirst mnstr mnstrUpdated (monsters world)
      updatedWorld = world { monsters = updatedMonsters }
      updatedState = setCurrentWorld updatedWorld state
   in if mAttackWait mnstr
      then updatedState
      else if canShoot world mnstrUpdated state.player.position
             then shootPlayer updatedState mnstrUpdated
             else combat updatedState mnstrUpdated False

-- A shot from across the room.
--
-- Unlike a blow traded at arm's length this costs the shooter nothing: the
-- player cannot swing back at something they are not standing next to, and
-- that asymmetry is the whole of what a bow is worth.
shootPlayer :: GameState -> Monster -> GameState
shootPlayer state mnstr =
  let plyr = player state
      (damage, rolled) =
        withRandom (rollDamage (mAttack mnstr - resistance plyr)) state
      (left, pack, rescued) = catchDeath rolled (health plyr - damage)
      told
        | damage <= 0 = [mName mnstr ++ " shoots at you, and misses."]
        | otherwise = [mName mnstr ++ " shoots you for " ++ show damage ++ " damage!"]
   in rolled
        { player = plyr {health = max 0 left, inventory = pack}
        , gameOver = left <= 0
        , message = rescued
                    ++ ["You have died! Game Over." | left <= 0]
                    ++ told
                    ++ message rolled
        }

-- A charm that catches you once is spent doing so.
--
-- Every death it can reach goes through here: a monster's blow, the
-- counterblow from your own attack, and a blade in the floor. The trap case
-- used to kill outright, which made "saves you from one death" mean "saves
-- you from one death unless the floor does it".
catchDeath :: GameState -> Int -> (Int, [Item], [String])
catchDeath state wounded
  | wounded > 0 = (wounded, pack, [])
  | otherwise = case carrying Revive state of
      Just charm ->
        ( maxHealth state
        , filter (/= charm) pack
        , [iName charm ++ " burns up, and you are standing again."]
        )
      Nothing -> (wounded, pack, [])
  where
    pack = inventory (player state)

-- Can anything walk over this tile, leaving aside who is standing on it?
isWalkable :: World -> V2 Int -> Bool
isWalkable world pos =
  gridLookup (mapGrid world) pos `notElem` [Nothing, Just Wall]
    && not (any (\d -> dePosition d == pos && deLocked d) (doors world))

-- How many steps each tile is from the player, out to a limit.
--
-- Monsters used to step in whichever direction shortened the straight line
-- to the player, which walks them into a wall and holds them there for as
-- long as the player stays behind it. Searching outwards from the player
-- instead gives every monster the real distance, so they can follow it
-- downhill and round the corner. One search serves the whole level.
approachField :: World -> V2 Int -> Int -> Map.Map (V2 Int) Int
approachField world from limit = spread (Map.singleton from 0) [from] 0
  where
    spread known frontier depth
      | depth >= limit || null frontier = known
      | otherwise =
          let found =
                [ next
                | pos <- frontier
                , next <- orthogonal pos
                , isWalkable world next
                , not (Map.member next known)
                ]
              fresh = Map.fromList [(pos, depth + 1) | pos <- found]
           in spread (Map.union known fresh) (Map.keys fresh) (depth + 1)

-- Move monsters in the current level
moveMonsters :: GameState -> GameState
moveMonsters state =
  let world = currentWorld state
      playerPos = state.player.position
      field = approachField world playerPos defaultMonsterRadius
      (inactiveMonsters, activeMonsters) = partition mInactive (monsters world)
      monsterPositions = map mPosition activeMonsters
      npcPositions = map npcPosition (npcs world)
      initialOccupiedPositions = playerPos : npcPositions ++ monsterPositions
      (updatedMonsters, _) =
        foldl
          (\(moved, occupied) monster ->
             let orgMonsterPos = mPosition monster
                 newMonster = moveMonsterWithOccupied world field playerPos occupied monster
                 newOccupied = replaceFirst orgMonsterPos (mPosition newMonster) occupied
             in (moved ++ [newMonster], newOccupied))
          ([], initialOccupiedPositions)
          activeMonsters

      updatedWorld = world { monsters = updatedMonsters ++ inactiveMonsters }
  in if playerIsHidden state
       then state -- monsters mill about rather than close in
       else setCurrentWorld updatedWorld state

-- Replace the first occurrence of a value in a list, leaving any later
-- occurrences alone
replaceFirst :: Eq a => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst old new (x:xs)
  | old == x  = new:xs
  | otherwise = x:replaceFirst old new xs

-- Take one step along the shortest way to the player.
--
-- A monster outside the field is either too far off or walled away from the
-- player entirely, and in both cases has no business giving chase.
moveMonsterWithOccupied :: World -> Map.Map (V2 Int) Int -> V2 Int -> [V2 Int] -> Monster -> Monster
moveMonsterWithOccupied world field playerPos occupiedPositions monster
  -- Something that shoots has no reason to walk into arm's reach. It stands
  -- where it is as long as it has the shot, which is what makes closing the
  -- distance the player's problem rather than its own.
  | canShoot world monster playerPos = monster
  | isAdjacent monsterPos playerPos = monster -- close enough to swing
  | otherwise = case closer of
      [] -> monster -- nowhere better to be
      (step : _) -> monster {mPosition = step, mAttackWait = isAdjacent step playerPos}
  where
    monsterPos = mPosition monster
    -- Ties break the same way every time, so a level plays out repeatably.
    closer = case Map.lookup monsterPos field of
      Nothing -> []
      Just here ->
        map snd $
          sortOn fst
            [ (there, next)
            | next <- orthogonal monsterPos
            , next `notElem` occupiedPositions
            , Just there <- [Map.lookup next field]
            , there < here
            ]

moveNPCs :: GameState -> GameState
moveNPCs state =
  let world = currentWorld state
      playerPos = state.player.position
      occupiedPositions = playerPos : map mPosition (monsters world)
                              ++ map npcPosition (npcs world)
      updatedNPCs = map (moveNPCWithOccupied world occupiedPositions playerPos) (npcs world)
      updatedWorld = world { npcs = updatedNPCs }
  in setCurrentWorld updatedWorld state

moveNPCWithOccupied :: World -> [V2 Int] -> V2 Int -> NPC -> NPC
moveNPCWithOccupied world occupiedPositions playerPos npc =
  let npcPos = npcPosition npc
      directions = [(0, 1, South), (1, 0, East), (0, -1, North), (-1, 0, West)] -- Possible directions
      currentDirection = npcPreferredDirection npc
      preferredMove =
        case currentDirection of
          Just dir -> find (\(_, _, d) -> d == dir) directions
          Nothing  -> Nothing
      allValidMoves =
        filter (\(dx, dy, _) -> isValidMove world playerPos (npcPos + V2 dx dy) && (npcPos + V2 dx dy) `notElem` occupiedPositions)
               directions
      newPreferredMove = case allValidMoves of
                           []    -> Nothing
                           (m:_) -> Just m
      selectedMove = if preferredMove `elem` map Just allValidMoves then preferredMove else newPreferredMove
  in case selectedMove of
       Just (dx, dy, newDir) -> npc { npcPosition = npcPos + V2 dx dy, npcPreferredDirection = Just newDir }
       Nothing -> npc -- No valid moves, stay in place

-- Check if a position is valid for monster movement
isValidMove :: World -> V2 Int -> V2 Int -> Bool
isValidMove world playerPos pos =
  let V2 x y = pos
      grid = mapGrid world
      doorAt = find (\d -> dePosition d == pos) (doors world)
      activeMonsters = filter (not . mInactive) (monsters world) -- Only active monsters block movement
  in y >= 0 && y < mapRows world &&
     x >= 0 && x < mapCols world &&
     (grid !! y !! x) /= Wall && -- Not a wall
     pos /= playerPos &&              -- Not the player's position
     not (any (\m -> mPosition m == pos) activeMonsters) && -- Check active monsters
     case doorAt of
       Just door -> not (deLocked door) -- Locked doors block movement
       Nothing   -> True -- No door, movement is allowed

processTriggers :: GameState -> GameState
processTriggers state =
  let world = currentWorld state
      (activated, remaining) = partition (\t -> evalTriggerCondition (triggerCondition t) state) (triggers world)
      recurringTriggers = filter triggerRecurring activated
      newState = foldl' executeTrigger state activated
  in withCurrentWorld (\w -> w { triggers = remaining ++ recurringTriggers }) newState

executeTrigger :: GameState -> Trigger -> GameState
executeTrigger state trigger = foldl' executeAction state (triggerActions trigger)

executeAction :: GameState -> Action -> GameState
executeAction state (SpawnItem name pos) =
  let world = currentWorld state
      updatedItems = map (\item -> if iName item == name && iPosition item == pos
                                   then item { iInactive = False }
                                   else item) (items world)
      updatedWorld = world { items = updatedItems }
   in setCurrentWorld updatedWorld state

executeAction state (SpawnMonster name pos) =
  let world = currentWorld state
      (inactiveMonsters, activeMonsters) = partition mInactive (monsters world)
      maybeTemplate = find (\m -> mName m == name) inactiveMonsters
  in case maybeTemplate of
       Just template ->
         let newMonster = template { mPosition = pos, mInactive = False }
             updatedWorld = world { monsters = newMonster : (activeMonsters ++ inactiveMonsters) }
         in setCurrentWorld updatedWorld state
       Nothing ->
         state { message = ("No inactive monster template found for " ++ name) : message state }

executeAction state (UnlockDoor pos) =
  let world = currentWorld state
      updatedDoors = map (\d -> if dePosition d == pos then d { deLocked = False } else d) (doors world)
      updatedWorld = world { doors = updatedDoors }
   in setCurrentWorld updatedWorld state

-- A trap. Enough of them and the run ends, so they are a real cost rather
-- than scenery.
executeAction state (HarmPlayer amount) =
  let struck = health (player state) - amount
      (hurt, pack, rescued) = catchDeath state struck
      -- Leave the thing that did it on the map. A line in the log scrolls
      -- away; a mark on the floor is still there when the player looks up.
      marked = withCurrentWorld
        (\w -> w {sprung = addCorpse (position (player state)) (sprung w)}) state
   in marked { player = (player state) { health = max 0 hurt, inventory = pack }
            , gameOver = hurt <= 0
              -- Newest first, so the order here is the reverse of the order
              -- it happened in: the blade, then whatever came of it.
            , message = rescued
                        ++ ["You have died! Game Over." | hurt <= 0]
                        ++ ("You take " ++ show amount ++ " damage!")
                        : message marked }

executeAction state (HealPlayer amount) =
  let mended = min (maxHealth state) (health (player state) + amount)
   in state { player = (player state) { health = mended }
            , message = ("You feel better, and recover "
                         ++ show (mended - health (player state)) ++ " HP.") : message state }

executeAction state (DisplayMessage msg) =
  state { message = msg : message state }

executeAction state (ShiftTile pos newTile) =
  let world = currentWorld state
      updatedOverrides = (pos, newTile) : filter ((/= pos) . fst) (tileOverrides world)
      updatedMap = updateTile (mapGrid world) pos newTile
      updatedWorld = world { mapGrid = updatedMap, tileOverrides = updatedOverrides }
   in setCurrentWorld updatedWorld state

executeAction state (TransportPlayer pos) =
  let world = currentWorld state
      updatedPlayer = (player state) { position = pos }
      updatedWorld = updateVisibility updatedPlayer defaultFogRadius world
   in setCurrentWorld updatedWorld $ state { player = updatedPlayer }

executeAction state (ConsumeItem itemName) =
  let plyr = player state
      updatedInventory = filter (\item -> iName item /= itemName) (inventory plyr)
      updatedPlayer = plyr { inventory = updatedInventory }
  in state { player = updatedPlayer
           , message = ("Consumed item: " ++ itemName) : message state }

executeAction state (AddToInventory itemName) =
  let world = currentWorld state
      (matchingItems, remainingItems) =
         partition (\item -> iName item == itemName && iInactive item) (items world)
   in case matchingItems of
        [] -> state { message = ("Item not found: " ++ itemName) : message state }
        (item:_) ->
           let updatedPlayer = (player state) { inventory = item : state.player.inventory }
               updatedWorld = world { items = remainingItems }
            in setCurrentWorld updatedWorld $ state
                 { player = updatedPlayer
                 , message = ("Added " ++ itemName ++ " to your inventory.") : message state }

executeAction state SetGameWon =
  state { gameWon = True, message = "Congratulations! You have won the game!" : message state }

--executeAction _ _ = error "Undefined trigger action"
