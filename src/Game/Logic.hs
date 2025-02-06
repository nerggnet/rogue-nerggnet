-- src/Game/Logic.hs
module Game.Logic where

import Brick
import Graphics.Vty (Key(..))
import Game.State (defaultMonsterRadius, defaultFogRadius, maxInventorySize, updateVisibility, replaceLevel, manhattanDistance, initGame)
import Game.GridUtils (updateTile)
import File.MapIO (loadNewGame)
import UI.Draw
import qualified Game.Types as Game
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, partition)

-- Handle movement keys
handleMovement :: Key -> EventM () Game.GameState ()
handleMovement key = do
  isAiming <- gets (maybe False (const True) . Game.aimingState)
  if isAiming
    then handleCommandInput key
    else do
      isGameOver <- gets Game.gameOver
      isGameWon <- gets Game.gameWon
      modify $ case key of
          KChar '?' -> \s -> s { Game.showLegend = not (Game.showLegend s) }
          KChar c | c == 'w' || c == 'k' -> if isGameWon || isGameOver then id else movePlayer Game.North
          KChar c | c == 's' || c == 'j' -> if isGameWon || isGameOver then id else movePlayer Game.South
          KChar c | c == 'a' || c == 'h' -> if isGameWon || isGameOver then id else movePlayer Game.West
          KChar c | c == 'd' || c == 'l' -> if isGameWon || isGameOver then id else movePlayer Game.East
          KChar '<' -> if isGameWon || isGameOver then id else goUp
          KChar '>' -> if isGameWon || isGameOver then id else goDown
          KChar 'g' -> if isGameWon || isGameOver then id else pickUpItem
          KChar 'u' -> if isGameWon || isGameOver then id else promptUseItem
          KChar 'x' -> if isGameWon || isGameOver then id else promptDropItem
          KChar ':' -> \s -> s { Game.commandMode = True, Game.commandBuffer = ":" }
          _ -> id
      modify $ if isGameWon || isGameOver then id else moveMonsters
      modify $ if isGameWon || isGameOver then id else monstersAttack
      modify $ if isGameWon || isGameOver then id else processTriggers
      -- Move NPCs every third keypress
      kyprssCnt <- gets Game.keyPressCount
      when (kyprssCnt == 0) $ modify moveNPCs
      modify (\s -> s { Game.message = take 10 (Game.message s) })

-- Go up stairs
goUp :: Game.GameState -> Game.GameState
goUp state =
  let playerPos = Game.position (Game.player state)
      currentWorld = Game.levels state !! Game.currentLevel state
      tile = (Game.mapGrid currentWorld) !! (playerPos ^. _y) !! (playerPos ^. _x)
  in case tile of
       Game.UpStair ->
         if Game.currentLevel state > 0
         then
           let newLevel = Game.currentLevel state - 1
               updatedWorld = updateVisibility (Game.player state) defaultFogRadius (Game.levels state !! newLevel)
            in state { Game.currentLevel = newLevel
                     , Game.levels = replaceLevel state newLevel updatedWorld
                     , Game.message = "You ascend the stairs." : Game.message state }
         else state { Game.message = "You are already on the top level." : Game.message state }
       _ -> state { Game.message = "No stairs to go up here!" : Game.message state }

-- Go down stairs
goDown :: Game.GameState -> Game.GameState
goDown state =
  let playerPos = Game.position (Game.player state)
      currentWorld = Game.levels state !! Game.currentLevel state
      tile = (Game.mapGrid currentWorld) !! (playerPos ^. _y) !! (playerPos ^. _x)
  in case tile of
       Game.DownStair ->
         if Game.currentLevel state < length (Game.levels state) - 1
         then
           let newLevel = Game.currentLevel state + 1
               updatedWorld = updateVisibility (Game.player state) defaultFogRadius (Game.levels state !! newLevel)
            in state { Game.currentLevel = newLevel
                     , Game.levels = replaceLevel state newLevel updatedWorld
                     , Game.message = "You descend the stairs." : Game.message state }
         else state { Game.message = "You are already on the bottom level." : Game.message state }
       _ -> state { Game.message = "No stairs to go down here!" : Game.message state }

pickUpItem :: Game.GameState -> Game.GameState
pickUpItem state =
  let currentWorld = Game.levels state !! Game.currentLevel state
      playerPos = Game.position (Game.player state)
      inventorySize = length (Game.inventory (Game.player state))
      (itemsOnTile, remainingItems) =
          partition (\item -> Game.iPosition item == playerPos && not (Game.iInactive item)) (Game.items currentWorld)
  in case itemsOnTile of
       [] -> state { Game.message = "There is nothing to pick up here." : Game.message state }
       (item:_) ->
         let existingStackableItem = find
               (\invItem -> Game.iName invItem == Game.iName item
                        && Game.iCategory invItem == Game.iCategory item
                        && Game.iEffectValue invItem == Game.iEffectValue item
                        && Game.iUses invItem /= Nothing)
               (Game.inventory (Game.player state))

             (invFull, invMsgs, updatedInventory) = case (existingStackableItem, Game.iUses item) of
               (Just invItem, Just uses) ->
                 (False, ["You picked up: " ++ Game.iName item], map (\i -> if i == invItem
                            then i { Game.iUses = fmap (+ uses) (Game.iUses i) }
                            else i)
                     (Game.inventory (Game.player state)))
               _ -> if inventorySize >= maxInventorySize
                    then (True, ["Your inventory is full! Drop an item before picking up more."], Game.inventory (Game.player state))
                    else (False, ["You picked up: " ++ Game.iName item], item : Game.inventory (Game.player state))  -- Add as a new item if not stackable

             updatedPlayer = (Game.player state) { Game.inventory = updatedInventory }
             updatedWorld = if invFull then currentWorld else currentWorld { Game.items = remainingItems }
         in state
              { Game.player = updatedPlayer
              , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld
              , Game.message =  invMsgs ++ Game.message state
              }

-- Player has requested to use an item, prompt which item to use
promptUseItem :: Game.GameState -> Game.GameState
promptUseItem state =
  let inv = Game.inventory (Game.player state)
  in if null inv
       then state { Game.message = "Your inventory is empty." : Game.message state }
       else state { Game.message = "Press a key to use an item." : Game.message state
                  , Game.commandMode = True
                  , Game.inventoryMode = Just Game.UseMode }

-- Player has requested to drop an item, prompt which item to drop
promptDropItem :: Game.GameState -> Game.GameState
promptDropItem state =
  let inv = Game.inventory (Game.player state)
  in if null inv
       then state { Game.message = "Your inventory is empty." : Game.message state }
       else state { Game.message = "Press a key to drop an item." : Game.message state
                  , Game.commandMode = True
                  , Game.inventoryMode = Just Game.DropMode }

-- Handle the usage of an item from the inventory
useItem :: Game.Item -> Game.GameState -> Game.GameState
useItem itm state =
  let plyr = Game.player state
      doorToUnlock = find (isAdjacent (Game.position plyr) . Game.dePosition)
                          (Game.doors (Game.levels state !! Game.currentLevel state))
      recalculateEffectiveStats p = p
        { Game.attack = Game.baseAttack p + maybe 0 Game.iEffectValue (Game.equippedWeapon p)
        , Game.resistance = Game.baseResistance p + maybe 0 Game.iEffectValue (Game.equippedArmor p) }

      -- Helper to reduce item uses or remove it if depleted
      reduceUses inventory =
        filter (\item -> Game.iUses item /= Just 0) $
        map (updateUses itm) inventory

      updatedState = case Game.iCategory itm of
        Game.Healing ->
          let playerCurrentMaxHealth = Game.xpHealth (Game.xpLevels state !! (Game.playerXPLevel plyr - 1))
              healedHealth = min playerCurrentMaxHealth (Game.health plyr + Game.iEffectValue itm)
           in state { Game.player = plyr { Game.health = healedHealth
                                        , Game.inventory = reduceUses (Game.inventory plyr) }
                    , Game.message = ("You used " ++ Game.iName itm ++ " and recovered "
                                     ++ show (Game.iEffectValue itm) ++ " HP.") : Game.message state }
        Game.Key ->
          case doorToUnlock of
            Just door ->
              let updatedDoors = map (\d -> if d == door then d { Game.deLocked = False } else d)
                                      (Game.doors (Game.levels state !! Game.currentLevel state))
                  updatedWorld = (Game.levels state !! Game.currentLevel state) { Game.doors = updatedDoors }
               in state { Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld
                        , Game.player = plyr { Game.inventory = reduceUses (Game.inventory plyr) }
                        , Game.message = ("You used " ++ Game.iName itm ++ " to unlock a door!") : Game.message state }
            Nothing ->
              state { Game.message = "There is no door nearby to unlock." : Game.message state }
        Game.Weapon ->
          let newPlayer = if Just itm == Game.equippedWeapon plyr
                          then plyr { Game.equippedWeapon = Nothing }
                          else plyr { Game.equippedWeapon = Just itm }
              newPlayerWithNewAttack = recalculateEffectiveStats newPlayer
              message = if Just itm == Game.equippedWeapon plyr
                        then "You unequipped " ++ Game.iName itm ++ "."
                        else "You equipped " ++ Game.iName itm ++ "."
           in state { Game.player = newPlayerWithNewAttack
                    , Game.message = message : Game.message state }
        Game.Range ->
          state { Game.aimingState = Just (Game.AimingState itm)
                , Game.commandMode = True
                , Game.message = ("You prepare to use " ++ Game.iName itm ++ ". Press a key to aim.") : Game.message state }
        Game.Armor  ->
          let newPlayer = if Just itm == Game.equippedArmor plyr
                          then plyr { Game.equippedArmor = Nothing }
                          else plyr { Game.equippedArmor = Just itm }
              newPlayerWithNewResistance = recalculateEffectiveStats newPlayer
              message = if Just itm == Game.equippedArmor plyr
                        then "You unequipped " ++ Game.iName itm ++ "."
                        else "You equipped " ++ Game.iName itm ++ "."
           in state { Game.player = newPlayerWithNewResistance
                    , Game.message = message : Game.message state }
        Game.Special ->
          state { Game.message = ("You used " ++ Game.iName itm ++ ". Its effect is mysterious.")
                               : Game.message state }
   in updatedState { Game.inventoryMode = Nothing }

-- Helper function: Check adjacency
isAdjacent :: V2 Int -> V2 Int -> Bool
isAdjacent (V2 x1 y1) (V2 x2 y2) =
  abs (x1 - x2) + abs (y1 - y2) == 1

-- Handle the case where the player wants to drop an item from the inventory
dropItem :: Game.Item -> Game.GameState -> Game.GameState
dropItem item state =
  let plyr = Game.player state
      currentWorld = Game.levels state !! Game.currentLevel state
      playerPos = Game.position plyr
      itemsOnTile = filter (\i -> Game.iPosition i == playerPos && not (Game.iInactive i)) (Game.items currentWorld)

  in if not (null itemsOnTile)
     then state { Game.message = "You cannot drop an item here, the space is occupied!" : Game.message state }
     else
       let updatedInventory = filter (/= item) (Game.inventory plyr)
           droppedItem = item { Game.iPosition = playerPos, Game.iInactive = False }
           updatedWorld = currentWorld { Game.items = droppedItem : Game.items currentWorld }
       in state { Game.player = plyr { Game.inventory = updatedInventory }
                , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld
                , Game.message = ("You dropped: " ++ Game.iName item) : Game.message state
                , Game.inventoryMode = Nothing
                }

-- Range attack handling (getVisibleMonsters, monsterList, executeRangedAttack, calculateRangedDamage)
getVisibleMonsters :: Game.GameState -> [(Char, Game.Monster)]
getVisibleMonsters state =
  let currentWorld = Game.levels state !! Game.currentLevel state
      visibleMonsters = filter (\m ->
        let pos = Game.mPosition m
            x = pos ^. _x
            y = pos ^. _y
        in Game.visibility currentWorld !! y !! x) (Game.monsters currentWorld)
  in zip ['a'..] visibleMonsters

monsterList :: [(Char, Game.Monster)] -> String
monsterList monsters =
  unwords $ map (\(c, m) -> [c] ++ ": " ++ Game.mName m) monsters

-- Helper to update item uses
updateUses :: Game.Item -> Game.Item -> Game.Item
updateUses usedItem item
  | item == usedItem = item { Game.iUses = fmap (\n -> n - 1) (Game.iUses item) }
  | otherwise = item

executeRangedAttack :: Game.GameState -> Game.Monster -> Game.Item -> Game.GameState
executeRangedAttack state targetMonster rangedItem =
  let damage = calculateRangedDamage (Game.player state) targetMonster rangedItem
      currentWorld = Game.levels state !! Game.currentLevel state
      updatedMonsters = map (\m -> if m == targetMonster then m { Game.mHealth = Game.mHealth m - damage } else m) (Game.monsters currentWorld)
      (defeatedMonsters, remainingMonsters) = partition ((<= 0) . Game.mHealth) updatedMonsters

      -- Mark the position where the monster was defeated
      updatedMapGrid =
        if Game.mHealth targetMonster - damage <= 0
        then
          let V2 mx my = Game.mPosition targetMonster
          in updateTile (Game.mapGrid currentWorld) (mx, my) (Game.Death)
        else Game.mapGrid currentWorld
      updatedWorld = currentWorld { Game.monsters = remainingMonsters, Game.mapGrid = updatedMapGrid }

      defeatMessage = if not (null defeatedMonsters)
                      then "You defeated " ++ Game.mName targetMonster ++ "!"
                      else ""
      attackMessage = "You hit " ++ Game.mName targetMonster ++ " for " ++ show damage ++ " damage!"
      (updatedPlayer, levelUpMessages) = if not (null defeatedMonsters)
                                         then
                                           let totalXP = sum (map Game.mXP defeatedMonsters)
                                               playerWithXP = (Game.player state) { Game.xp = Game.xp (Game.player state) + totalXP }
                                           in levelUp playerWithXP (Game.xpLevels state)
                                         else (Game.player state, [])
      xpGainMessage = if not (null defeatedMonsters)
                      then "You gained " ++ show (sum (map Game.mXP defeatedMonsters)) ++ " XP!"
                      else ""
      completeMessages = levelUpMessages ++ [defeatMessage, xpGainMessage, attackMessage]
      updatedPlayerWithReducedUsesForItem = updatedPlayer { Game.inventory = filter (\item -> Game.iUses item /= Just 0) $ map (updateUses rangedItem) (Game.inventory updatedPlayer) }
  in state { Game.player = updatedPlayerWithReducedUsesForItem
           , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld
           , Game.message = completeMessages ++ Game.message state }

calculateRangedDamage :: Game.Player -> Game.Monster -> Game.Item -> Int
calculateRangedDamage player monster rangedItem =
  let baseDamage = Game.attack player
      rangedBonus = Game.iEffectValue rangedItem
      monsterResistance = max 0 (Game.mHealth monster `div` 10) -- Example: Monster's resistance based on health
      totalDamage = max 0 (baseDamage + rangedBonus - monsterResistance)
  in totalDamage

handleCommandInput :: Key -> EventM () Game.GameState ()
handleCommandInput key = do
  state <- get
  let (keyChar, escPressed) = case key of
        KChar c -> (Just c, False)
        KEsc    -> (Nothing, True)
        KEnter  -> (Just '\n', False)
        KBS     -> (Just '\b', False)
        _       -> (Nothing, False)
  let stateModifier = handleCommandInputInternal keyChar escPressed state
  modify stateModifier

  -- Check to see if there is a command to execute
  newState <- get
  when (Game.commandToExecute newState) $ do
    executeCommand (Game.commandBuffer newState)
    modify (\s -> s { Game.commandToExecute = False, Game.commandBuffer = "" })  -- Clear buffer after execution

handleCommandInputInternal :: Maybe Char -> Bool -> Game.GameState -> (Game.GameState -> Game.GameState)
handleCommandInputInternal key esc state =
  case Game.aimingState state of
    Just (Game.AimingState rangedItem) ->
      case key of
        Just c | Just monster <- lookup c (getVisibleMonsters state) ->
          \s -> exitAimingMode (executeRangedAttack s monster rangedItem)
        _ | esc -> exitAimingMode
        _ -> addMessage "Invalid selection. Press ESC to cancel."
    Nothing ->
      if null (Game.commandBuffer state)
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
    exitAimingMode s = s { Game.aimingState = Nothing, Game.commandMode = False }
    exitCommandMode s = s { Game.commandMode = False }
    exitCommandModeAndClearBuffer s = s { Game.commandMode = False, Game.commandBuffer = "" }
    appendToCommandBuffer c s = s { Game.commandBuffer = Game.commandBuffer s ++ [c] }
    removeLastCommandChar s = s { Game.commandBuffer = initSafe (Game.commandBuffer s) }
    addMessage msg s = s { Game.message = msg : Game.message s }
    markCommandForExecution s = s { Game.commandMode = False, Game.commandToExecute = True }

    processInventorySelection c s =
      let inventory = Game.inventory (Game.player s)
          eqpdWeapon = Game.equippedWeapon (Game.player s)
          eqpdArmor = Game.equippedArmor (Game.player s)
      in case lookup c (keyedInventory inventory eqpdWeapon eqpdArmor) of
        Just item -> case Game.inventoryMode s of
          Just Game.UseMode  -> exitCommandMode $ useItem item s
          Just Game.DropMode -> exitCommandMode $ dropItem item s
          Nothing -> addMessage "Use/Drop error" s
        Nothing -> addMessage "Invalid selection." s

-- Execute commands
executeCommand :: String -> EventM () Game.GameState ()
executeCommand ":q" = halt -- Quit the game
executeCommand ":restart" = do -- Restart the game
  newState <- liftIO loadNewGame
  case newState of
    Left _ -> put $ initGame newState -- $ Left (config { Game.message = ["Game restarted!"] } )
    Right state -> put state
executeCommand ":heal" = do -- Cheat
    state <- get
    let plyr = Game.player state
        playerCurrentMaxHealth = Game.xpHealth (Game.xpLevels state !! (Game.playerXPLevel plyr - 1))
    modify (\s -> s { Game.player = plyr { Game.health = playerCurrentMaxHealth }, Game.gameOver = False, Game.commandToExecute = False } )
executeCommand ":super" = do -- Cheat a lot
    state <- get
    let plyr = Game.player state
    modify (\s -> s { Game.player = plyr { Game.health = 1000, Game.attack = 100, Game.resistance = 100 }, Game.gameOver = False, Game.commandToExecute = False } )
executeCommand cmd  = modify (\s -> s { Game.message = ("Unknown command: " ++ cmd) : Game.message s, Game.commandToExecute = False })

-- Safe init for empty lists
initSafe :: [a] -> [a]
initSafe [] = []
initSafe xs = init xs

-- Move the player in a direction
movePlayer :: Game.Direction -> Game.GameState -> Game.GameState
movePlayer dir state =
  let playerPos = Game.position (Game.player state)
      currentWorld = Game.levels state !! Game.currentLevel state
      worldMap = Game.mapGrid currentWorld
      newPos = case dir of
        Game.North -> playerPos + V2 0 (-1)
        Game.South -> playerPos + V2 0 1
        Game.West  -> playerPos + V2 (-1) 0
        Game.East  -> playerPos + V2 1 0
        _          -> playerPos

      -- Helper to find an active monster at a given position
      monsterAt pos = find (\m -> Game.mPosition m == pos) (filter (not . Game.mInactive) (Game.monsters currentWorld))

      -- Helper to find a door at a given position
      doorAt pos = find (\d -> Game.dePosition d == pos) (Game.doors currentWorld)

      -- Check if the new position is occupied by an NPC
      npcAt pos = find (\npc -> Game.npcPosition npc == pos) (Game.npcs currentWorld)

      -- Helper to check if the player can move to a position
      canMove pos =
        let V2 x y = pos
        in y >= 0 && y < Game.mapRows currentWorld &&
           x >= 0 && x < Game.mapCols currentWorld &&
           (worldMap !! y !! x) /= Game.Wall

      -- Helper to handle movement
      internalHandleMovement nPos =
        let updatedWorld = updateVisibility (Game.player state) defaultFogRadius currentWorld
        in state { Game.player = (Game.player state) { Game.position = nPos }
                 , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }
  in case (doorAt newPos, monsterAt newPos, npcAt newPos) of
       (Just door, _, _) | Game.deLocked door -> -- Locked door case
         state { Game.message = ("The door in in front of you is locked and is blocking your way.") : Game.message state }
       (_, Nothing, Nothing) | canMove newPos -> -- No monster or NPC
         internalHandleMovement newPos
       (_, Just monster, _) -> -- Monster
         combat state monster True
       (_, _, Just npc) -> -- NPC
         state { Game.message = (Game.npcName npc ++ " says: " ++ Game.npcMessage npc) : Game.message state
               , Game.lastInteractedNpc = Just (Game.npcName npc)
               }
       _ -> state -- Invalid move

-- Player hits a monster and the monster returns the favor
combat :: Game.GameState -> Game.Monster -> Bool -> Game.GameState
combat state mnstr playerGoesFirst =
  let player = Game.player state
      playerDamage = Game.attack player
      monsterDamage = max 0 (Game.mAttack mnstr - Game.resistance player)
      newHealth = max 0 (Game.health player - monsterDamage)
      updatedPlayer = player { Game.health = newHealth }
      currentWorld = Game.levels state !! Game.currentLevel state
      updatedMonsters =
        if Game.mHealth mnstr - playerDamage <= 0
        then filter (/= mnstr) (Game.monsters currentWorld)
        else map (\m -> if m == mnstr then m { Game.mHealth = Game.mHealth mnstr - playerDamage } else m)
                 (Game.monsters currentWorld)

      -- Mark the position where the monster was defeated
      updatedMapGrid =
        if Game.mHealth mnstr - playerDamage <= 0
        then
          let V2 mx my = Game.mPosition mnstr
          in updateTile (Game.mapGrid currentWorld) (mx, my) (Game.Death)
        else Game.mapGrid currentWorld
      updatedWorld = currentWorld { Game.monsters = updatedMonsters, Game.mapGrid = updatedMapGrid }

      isDead = newHealth == 0
      defeatMessage = if Game.mHealth mnstr - playerDamage <= 0
                      then "You defeated the " ++ Game.mName mnstr ++ " and gained " ++ show (Game.mXP mnstr) ++ " XP!"
                      else ""
      attackMessage = if playerGoesFirst
                      then "You attacked " ++ Game.mName mnstr ++ " for " ++ show playerDamage ++ " damage!"
                      else "The " ++ Game.mName mnstr ++ " attacked you for " ++ show monsterDamage ++ " damage!"
      counterattackMessage = if playerGoesFirst
                             then "The " ++ Game.mName mnstr ++ " counterattacked you for " ++ show monsterDamage ++ " damage!"
                             else "You counterattacked " ++ Game.mName mnstr ++ " for " ++ show playerDamage ++ " damage!"
      deadMessage = if isDead then "You have died! Game Over." else ""
      combatMessages = deadMessage : defeatMessage : counterattackMessage : attackMessage : []
      updatedPlayerWithXP = if Game.mHealth mnstr - playerDamage <= 0
                            then updatedPlayer { Game.xp = Game.xp updatedPlayer + Game.mXP mnstr }
                            else updatedPlayer
      (updatedPlayerWithXPAndPossibleNewLevel, levelUpMessages) =
          if isDead
          then (updatedPlayerWithXP, [])
          else levelUp updatedPlayerWithXP (Game.xpLevels state)
      completeMessage = levelUpMessages ++ combatMessages ++ Game.message state
  in if Game.mInactive mnstr
     then state
     else state { Game.player = updatedPlayerWithXPAndPossibleNewLevel
                , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld
                , Game.message = completeMessage
                , Game.gameOver = isDead }

levelUp :: Game.Player -> [Game.XPLevel] -> (Game.Player, [String])
levelUp player xpLevels =
  let currentXP = Game.xp player
      currentXPLevel = Game.playerXPLevel player
      nextXPLevel = find (\lvl -> currentXP >= Game.xpThreshold lvl && Game.xpLevel lvl > currentXPLevel) xpLevels
      calculateEffectiveStats plyr = plyr
        { Game.attack = Game.baseAttack plyr + maybe 0 Game.iEffectValue (Game.equippedWeapon plyr)
        , Game.resistance = Game.baseResistance plyr + maybe 0 Game.iEffectValue (Game.equippedArmor plyr)
        }
  in case nextXPLevel of
       Just lvl ->
         let updatedPlayer = player
               { Game.playerXPLevel = Game.xpLevel lvl
               , Game.health = Game.xpHealth lvl
               , Game.baseAttack = Game.xpAttack lvl
               , Game.baseResistance = Game.xpResistance lvl
               }
             recalculatedPlayer = calculateEffectiveStats updatedPlayer
         in (recalculatedPlayer,
             [ "You leveled up to level " ++ show (Game.xpLevel lvl) ++ "!"
             , "Health increased to " ++ show (Game.xpHealth lvl) ++ "."
             , "Base attack increased to " ++ show (Game.xpAttack lvl) ++ "."
             , "Base resistance increased to " ++ show (Game.xpResistance lvl) ++ "."
             ])
       Nothing -> (player, [])

-- Monsters in tiles adjacent to the player should attack
monstersAttack :: Game.GameState -> Game.GameState
monstersAttack state =
  let currentWorld = Game.levels state !! Game.currentLevel state
      playerPos = Game.position (Game.player state)
      (_, activeMonsters) = partition Game.mInactive (Game.monsters currentWorld)
      monstersAdjacentToPlayer = filter (\m ->
          let mPos = Game.mPosition m
           in isAdjacent mPos playerPos) activeMonsters
    in foldl' monsterAttackOrWait state monstersAdjacentToPlayer

-- Helper for handling either monster going into combat or monster waiting
monsterAttackOrWait :: Game.GameState -> Game.Monster -> Game.GameState
monsterAttackOrWait state mnstr =
  let currentWorld = Game.levels state !! Game.currentLevel state
      mnstrUpdated = mnstr { Game.mAttackWait = not (Game.mAttackWait mnstr) }
      updatedMonsters = replace mnstr mnstrUpdated (Game.monsters currentWorld)
      updatedWorld = currentWorld { Game.monsters = updatedMonsters }
      updatedState = state { Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }
   in if Game.mAttackWait mnstr
      then updatedState
      else combat updatedState mnstrUpdated False

-- Move monsters in the current level
moveMonsters :: Game.GameState -> Game.GameState
moveMonsters state =
  let currentWorld = Game.levels state !! Game.currentLevel state
      playerPos = Game.position (Game.player state)
      (inactiveMonsters, activeMonsters) = partition Game.mInactive (Game.monsters currentWorld)
      monsterPositions = map Game.mPosition activeMonsters
      npcPositions = map Game.npcPosition (Game.npcs currentWorld)
      initialOccupiedPositions = playerPos : npcPositions ++ monsterPositions
      (updatedMonsters, _) =
        foldl
          (\(monsters, occupied) monster ->
             let orgMonsterPos = Game.mPosition monster
                 newMonster = moveMonsterWithOccupied currentWorld playerPos occupied monster
                 newOccupied = replace orgMonsterPos (Game.mPosition newMonster) occupied
             in (monsters ++ [newMonster], newOccupied))
          ([], initialOccupiedPositions)
          activeMonsters

      updatedWorld = currentWorld { Game.monsters = updatedMonsters ++ inactiveMonsters }
  in state { Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }

-- Helper function to replace an item in a list
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs)
  | old == x  = new:xs
  | otherwise = x:replace old new xs

moveMonsterWithOccupied :: Game.World -> V2 Int -> [V2 Int] -> Game.Monster -> Game.Monster
moveMonsterWithOccupied world playerPos occupiedPositions monster =
  let monsterPos = Game.mPosition monster
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
              in monster { Game.mPosition = newPos, Game.mAttackWait = newAttackWaiting } -- Move to the first valid position
         _ -> monster -- Stay in place if no valid moves

moveNPCs :: Game.GameState -> Game.GameState
moveNPCs state =
  let currentWorld = Game.levels state !! Game.currentLevel state
      playerPos = Game.position (Game.player state)
      occupiedPositions = playerPos : map Game.mPosition (Game.monsters currentWorld)
                              ++ map Game.npcPosition (Game.npcs currentWorld)
      updatedNPCs = map (moveNPCWithOccupied currentWorld occupiedPositions playerPos) (Game.npcs currentWorld)
      updatedWorld = currentWorld { Game.npcs = updatedNPCs }
  in state { Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }

moveNPCWithOccupied :: Game.World -> [V2 Int] -> V2 Int -> Game.NPC -> Game.NPC
moveNPCWithOccupied world occupiedPositions playerPos npc =
  let npcPos = Game.npcPosition npc
      directions = [(0, 1, Game.South), (1, 0, Game.East), (0, -1, Game.North), (-1, 0, Game.West)] -- Possible directions
      currentDirection = Game.npcPreferredDirection npc
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
      selectedMove = if preferredMove `elem` (map Just allValidMoves) then preferredMove else newPreferredMove
  in case selectedMove of
       Just (dx, dy, newDir) -> npc { Game.npcPosition = npcPos + V2 dx dy, Game.npcPreferredDirection = Just newDir }
       Nothing -> npc -- No valid moves, stay in place

-- Check if a position is valid for monster movement
isValidMove :: Game.World -> V2 Int -> V2 Int -> Bool
isValidMove world playerPos pos =
  let V2 x y = pos
      grid = Game.mapGrid world
      doorAt = find (\d -> Game.dePosition d == pos) (Game.doors world)
      activeMonsters = filter (not . Game.mInactive) (Game.monsters world) -- Only active monsters block movement
  in y >= 0 && y < Game.mapRows world &&
     x >= 0 && x < Game.mapCols world &&
     (grid !! y !! x) /= Game.Wall && -- Not a wall
     pos /= playerPos &&              -- Not the player's position
     not (any (\m -> Game.mPosition m == pos) activeMonsters) && -- Check active monsters
     case doorAt of
       Just door -> not (Game.deLocked door) -- Locked doors block movement
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

processTriggers :: Game.GameState -> Game.GameState
processTriggers state =
  let currentWorld = Game.levels state !! Game.currentLevel state
      (activated, remaining) = partition (\t -> Game.triggerCondition t state) (Game.triggers currentWorld)
      recurringTriggers = filter Game.triggerRecurring activated
      newState = foldl' executeTrigger state activated
      updatedCurrentWorld = (Game.levels newState !! Game.currentLevel newState) { Game.triggers = remaining ++ recurringTriggers }
  in newState { Game.levels = replaceLevel state (Game.currentLevel state) updatedCurrentWorld }

executeTrigger :: Game.GameState -> Game.Trigger -> Game.GameState
executeTrigger state trigger = foldl' executeAction state (Game.triggerActions trigger)

executeAction :: Game.GameState -> Game.Action -> Game.GameState
executeAction state (Game.SpawnItem name pos) =
  let currentWorld = Game.levels state !! Game.currentLevel state
      updatedItems = map (\item -> if Game.iName item == name && Game.iPosition item == pos
                                   then item { Game.iInactive = False }
                                   else item) (Game.items currentWorld)
      updatedWorld = currentWorld { Game.items = updatedItems }
   in state { Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }

executeAction state (Game.SpawnMonster name pos) =
  let currentWorld = Game.levels state !! Game.currentLevel state
      (inactiveMonsters, activeMonsters) = partition Game.mInactive (Game.monsters currentWorld)
      maybeTemplate = find (\m -> Game.mName m == name) inactiveMonsters
  in case maybeTemplate of
       Just template ->
         let newMonster = template { Game.mPosition = pos, Game.mInactive = False }
             updatedWorld = currentWorld { Game.monsters = newMonster : (activeMonsters ++ inactiveMonsters) }
         in state { Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }
       Nothing ->
         state { Game.message = ("No inactive monster template found for " ++ name) : Game.message state }

executeAction state (Game.UnlockDoor pos) =
  let currentWorld = Game.levels state !! Game.currentLevel state
      updatedDoors = map (\d -> if Game.dePosition d == pos then d { Game.deLocked = False } else d) (Game.doors currentWorld)
      updatedWorld = currentWorld { Game.doors = updatedDoors }
   in state { Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }

executeAction state (Game.DisplayMessage msg) =
  state { Game.message = msg : Game.message state }

executeAction state (Game.ShiftTile pos newTile) =
  let currentWorld = Game.levels state !! Game.currentLevel state
      updatedOverrides = (pos, newTile) : filter ((/= pos) . fst) (Game.tileOverrides currentWorld)
      updatedMap = updateTile (Game.mapGrid currentWorld) (pos ^. _x, pos ^. _y) newTile
      updatedWorld = currentWorld { Game.mapGrid = updatedMap, Game.tileOverrides = updatedOverrides }
   in state { Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }

executeAction state (Game.TransportPlayer pos) =
  let currentWorld = Game.levels state !! Game.currentLevel state
      updatedPlayer = (Game.player state) { Game.position = pos }
      updatedWorld = updateVisibility updatedPlayer defaultFogRadius currentWorld
   in state
       { Game.player = updatedPlayer
       , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }

executeAction state (Game.ConsumeItem itemName) =
  let plyr = Game.player state
      updatedInventory = filter (\item -> Game.iName item /= itemName) (Game.inventory plyr)
      updatedPlayer = plyr { Game.inventory = updatedInventory }
  in state { Game.player = updatedPlayer
           , Game.message = ("Consumed item: " ++ itemName) : Game.message state }

executeAction state (Game.AddToInventory itemName) =
  let currentWorld = Game.levels state !! Game.currentLevel state
      (matchingItems, remainingItems) =
         partition (\item -> Game.iName item == itemName && Game.iInactive item) (Game.items currentWorld)
   in case matchingItems of
        [] -> state { Game.message = ("Item not found: " ++ itemName) : Game.message state }
        (item:_) ->
           let updatedPlayer = (Game.player state) { Game.inventory = item : Game.inventory (Game.player state) }
               updatedWorld = currentWorld { Game.items = remainingItems }
            in state { Game.player = updatedPlayer
                     , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld
                     , Game.message = ("Added " ++ itemName ++ " to your inventory.") : Game.message state }

executeAction state Game.SetGameWon =
  state { Game.gameWon = True, Game.message = "Congratulations! You have won the game!" : Game.message state }

--executeAction _ _ = error "Undefined trigger action"
