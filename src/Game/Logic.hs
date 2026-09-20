-- src/Game/Logic.hs
module Game.Logic where

import Game.State
  ( defaultMonsterRadius, defaultFogRadius, maxInventorySize
  , updateVisibility, manhattanDistance, evalTriggerCondition, visibleMonsters
  , currentWorld, setCurrentWorld, withCurrentWorld, replaceLevel, maxLogMessages, maxHealth, npcMoveInterval, nextHelpPage, withRandom
  )
import Game.GridUtils (updateTile, gridLookup, keyedInventory)
import Game.Types
import Linear.V2 (V2(..))
import Data.List (find, partition)
import Data.Maybe (isJust)
import System.Random (StdGen, uniformR)

handleMovementInternal :: Maybe Char -> GameState -> GameState
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
  let ticked = state { keyPressCount = (keyPressCount state + 1) `mod` npcMoveInterval }
      state' = moveMonsters ticked
      state'' = monstersAttack state'
      state''' = processTriggers state''
      state'''' = if keyPressCount state''' == 0 then moveNPCs state''' else state'''
  in state'''' { message = take maxLogMessages (message state'''') }

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
                    , levels = replaceLevel state newLevel updatedWorld
                    , message = "You descend the stairs." : message state }
      | otherwise -> state { message = "You are already on the bottom level." : message state }
    _ -> state { message = "No stairs to go down here!" : message state }

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
               (Just invItem, Just uses) ->
                 (False, ["You picked up: " ++ iName item], map (\i -> if i == invItem
                            then i { iUses = fmap (+ uses) (iUses i) }
                            else i)
                     (state.player.inventory))
               _ -> if inventorySize >= maxInventorySize
                    then (True, ["Your inventory is full! Drop an item before picking up more."], state.player.inventory)
                    else (False, ["You picked up: " ++ iName item], item : state.player.inventory)  -- Add as a new item if not stackable

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
        Special ->
          state { message = ("You used " ++ iName itm ++ ". Its effect is mysterious.")
                               : message state }
   in updatedState { inventoryMode = Nothing }

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
       in setCurrentWorld updatedWorld $ rolled
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
      internalHandleMovement nPos =
        let updatedWorld = updateVisibility (player state) defaultFogRadius world
        in setCurrentWorld updatedWorld $
             state { player = (player state) { position = nPos } }
  in case (doorAt newPos, monsterAt newPos, npcAt newPos) of
       (Just door, _, _) | deLocked door -> -- Locked door case
         state { message = "The door in front of you is locked and is blocking your way." : message state }
       (_, Nothing, Nothing) | canMove newPos -> -- No monster or NPC
         internalHandleMovement newPos
       (_, Just monster, _) -> -- Monster
         combat state monster True
       (_, _, Just npc) -> -- NPC
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
          newHealth = max 0 (health plyr - monsterDamage)
          updatedPlayer = plyr { health = newHealth }
          monsterDefeated = mHealth target - playerDamage <= 0

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
          combatMessages = filter (not . null)
            [deadMessage, defeatMessage, counterattackMessage, attackMessage]
          updatedPlayerWithXP = if monsterDefeated
                                then updatedPlayer { xp = xp updatedPlayer + mXP target }
                                else updatedPlayer
          (updatedPlayerWithXPAndPossibleNewLevel, levelUpMessages) =
              if isDead
              then (updatedPlayerWithXP, [])
              else levelUp updatedPlayerWithXP (xpLevels rolled)
          completeMessage = levelUpMessages ++ combatMessages ++ message rolled
       in setCurrentWorld updatedWorld $ rolled
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

-- Monsters in tiles adjacent to the player should attack
monstersAttack :: GameState -> GameState
monstersAttack state =
  let world = currentWorld state
      playerPos = state.player.position
      (_, activeMonsters) = partition mInactive (monsters world)
      monstersAdjacentToPlayer = filter (\m ->
          let mPos = mPosition m
           in isAdjacent mPos playerPos) activeMonsters
    in foldl' monsterAttackOrWait state monstersAdjacentToPlayer

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
      else combat updatedState mnstrUpdated False

-- Move monsters in the current level
moveMonsters :: GameState -> GameState
moveMonsters state =
  let world = currentWorld state
      playerPos = state.player.position
      (inactiveMonsters, activeMonsters) = partition mInactive (monsters world)
      monsterPositions = map mPosition activeMonsters
      npcPositions = map npcPosition (npcs world)
      initialOccupiedPositions = playerPos : npcPositions ++ monsterPositions
      (updatedMonsters, _) =
        foldl
          (\(moved, occupied) monster ->
             let orgMonsterPos = mPosition monster
                 newMonster = moveMonsterWithOccupied world playerPos occupied monster
                 newOccupied = replaceFirst orgMonsterPos (mPosition newMonster) occupied
             in (moved ++ [newMonster], newOccupied))
          ([], initialOccupiedPositions)
          activeMonsters

      updatedWorld = world { monsters = updatedMonsters ++ inactiveMonsters }
  in setCurrentWorld updatedWorld state

-- Replace the first occurrence of a value in a list, leaving any later
-- occurrences alone
replaceFirst :: Eq a => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst old new (x:xs)
  | old == x  = new:xs
  | otherwise = x:replaceFirst old new xs

moveMonsterWithOccupied :: World -> V2 Int -> [V2 Int] -> Monster -> Monster
moveMonsterWithOccupied world playerPos occupiedPositions monster =
  let monsterPos = mPosition monster
      distance = manhattanDistance playerPos monsterPos
      potentialMoves =
        filter (\pos -> isValidMove world playerPos pos && pos `notElem` occupiedPositions)
               [V2 (x+dx) (y+dy) | (dx, dy) <- moveDirections]
        where V2 x y = monsterPos
      moveDirections =
        if distance <= defaultMonsterRadius
        then prioritizeTowardsPlayer playerPos monsterPos
        else [(0, 0)] -- Stay in place if out of range
  in if isAdjacent monsterPos playerPos
     then monster -- Stay if adjacent to player
     else
       case potentialMoves of
         (newPos:_) ->
             let newAttackWaiting = isAdjacent newPos playerPos
              in monster { mPosition = newPos, mAttackWait = newAttackWaiting } -- Move to the first valid position
         _ -> monster -- Stay in place if no valid moves

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

-- Prioritize movement directions towards the player
prioritizeTowardsPlayer :: V2 Int -> V2 Int -> [(Int, Int)]
prioritizeTowardsPlayer (V2 px py) (V2 mx my) =
  let dx = px - mx -- Horizontal distance to player
      dy = py - my -- Vertical distance to player
      horizontalFirst = [(signum dx, 0), (0, signum dy)]
      verticalFirst = [(0, signum dy), (signum dx, 0)]
  in if abs dx >= abs dy
     then horizontalFirst ++ [(signum dx, signum dy), (-signum dx, 0), (0, -signum dy)]
     else verticalFirst ++ [(signum dx, signum dy), (0, -signum dy), (-signum dx, 0)]

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
