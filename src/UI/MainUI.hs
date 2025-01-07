-- src/UI/MainUI.hs
{-# OPTIONS_GHC -Wno-x-partial #-}

module UI.MainUI (startGame) where

import Brick
import Graphics.Vty (Event(..), Key(..), rgbColor, withBackColor, withForeColor, defAttr, black, white, yellow, green, red, blue, magenta)
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import Game.State (initGame, defaultHealth, defaultMonsterAttack, updateVisibility, replaceLevel, manhattanDistance)
import UI.Draw
import qualified Game.Types as Game
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))
import Data.List (partition)

-- App definition
app :: App Game.GameState e ()
app = App
  { appDraw = drawUI
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const defaultAttrMap
  , appChooseCursor = chooseCursor
  }

chooseCursor :: Game.GameState -> [CursorLocation n] -> Maybe (CursorLocation n)
chooseCursor state =
  if Game.commandMode state
    then showFirstCursor state
    else neverShowCursor state

-- Main function to start the game
startGame :: IO ()
startGame = do
  initialState <- initGame
  let buildVty = mkVty defaultConfig
  vty <- buildVty
  _ <- customMain vty buildVty Nothing app initialState
  putStrLn "Game Over!"

-- Handle events
handleEvent :: BrickEvent () e -> EventM () Game.GameState ()
handleEvent (VtyEvent (EvKey key [])) = do
  isCommandMode <- gets Game.commandMode
  if isCommandMode
    then handleCommandInput key
    else handleMovement key
handleEvent _ = return ()

-- Handle movement keys
handleMovement :: Key -> EventM () Game.GameState ()
handleMovement key = do
  isGameOver <- gets Game.gameOver
  modify $ case key of
      KChar 'w' -> if isGameOver then id else movePlayer Game.North
      KChar 's' -> if isGameOver then id else movePlayer Game.South
      KChar 'a' -> if isGameOver then id else movePlayer Game.West
      KChar 'd' -> if isGameOver then id else movePlayer Game.East
      KChar '<' -> if isGameOver then id else goUp
      KChar '>' -> if isGameOver then id else goDown
      KChar 'g' -> if isGameOver then id else pickUpItem
      KChar 'u' -> if isGameOver then id else promptUseItem
      KChar ':' -> \s -> s { Game.commandMode = True, Game.commandBuffer = ":" }
      _         -> id
  modify moveMonsters

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
               updatedWorld = updateVisibility (Game.player state) 5 (Game.levels state !! newLevel)
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
               updatedWorld = updateVisibility (Game.player state) 5 (Game.levels state !! newLevel)
            in state { Game.currentLevel = newLevel
                     , Game.levels = replaceLevel state newLevel updatedWorld
                     , Game.message = "You descend the stairs." : Game.message state }
         else state { Game.message = "You are already on the bottom level." : Game.message state }
       _ -> state { Game.message = "No stairs to go down here!" : Game.message state }

-- Pick up an item at the player's position
pickUpItem :: Game.GameState -> Game.GameState
pickUpItem state =
  let currentWorld = Game.levels state !! Game.currentLevel state
      playerPos = Game.position (Game.player state)
      -- Separate items on the player's position from the rest
      (itemsOnTile, remainingItems) = partition (\item -> Game.iPosition item == playerPos) (Game.items currentWorld)
  in case itemsOnTile of
       [] -> state { Game.message = "There is nothing to pick up here." : Game.message state }
       (item:_) ->
         state
           { Game.levels = replaceLevel state (Game.currentLevel state) currentWorld { Game.items = remainingItems }
           , Game.player = (Game.player state) { Game.inventory = item : Game.inventory (Game.player state) }
           , Game.message = ("You picked up: " ++ Game.iName item) : Game.message state
           }

-- Player has requested to use an item, prompt which item to use
promptUseItem :: Game.GameState -> Game.GameState
promptUseItem state =
  let inv = Game.inventory (Game.player state)
  in if null inv
       then state { Game.message = "Your inventory is empty." : Game.message state }
       else state { Game.message = "Press a key to use an item." : Game.message state
                  , Game.commandMode = True }

-- Handle the usage of an item from the inventory
useItem :: Game.Item -> Game.GameState -> Game.GameState
useItem itm state =
  let plyr = Game.player state
      newState = case Game.iCategory itm of
        Game.Healing ->
          let healedHealth = min defaultHealth (Game.health plyr + Game.iEffectValue itm)
          in state { Game.player = plyr { Game.health = healedHealth
                                        , Game.inventory = filter (/= itm) (Game.inventory plyr) }
                   , Game.message = ("You used " ++ Game.iName itm ++ " and recovered "
                                    ++ show (Game.iEffectValue itm) ++ " HP.") : Game.message state }
        Game.Weapon ->
          let oldWeapon = Game.equippedWeapon plyr
              newAttack = Game.attack plyr
                          - maybe 0 Game.iEffectValue oldWeapon
                          + Game.iEffectValue itm
          in state { Game.player = plyr { Game.equippedWeapon = Just itm
                                        , Game.attack = newAttack }
                   , Game.message = ("You equipped " ++ Game.iName itm ++ ".") : Game.message state }
        Game.Armor  ->
          let oldArmor = Game.equippedArmor plyr
              newResistance = Game.resistance plyr
                              - maybe 0 Game.iEffectValue oldArmor
                              + Game.iEffectValue itm
          in state { Game.player = plyr { Game.equippedArmor = Just itm
                                        , Game.resistance = newResistance }
                   , Game.message = ("You equipped " ++ Game.iName itm ++ ".") : Game.message state }
        Game.Special ->
          state { Game.message = ("You used " ++ Game.iName itm ++ ". Its effect is mysterious.")
                               : Game.message state }
  in newState

-- Handle commands
handleCommandInput :: Key -> EventM () Game.GameState ()
handleCommandInput key = do
  state <- get
  if null (Game.commandBuffer state) -- Handle item selection if command buffer is empty
    then case key of
      KChar c -> do
        let inventory = Game.inventory (Game.player state)
        case lookup c (keyedInventory inventory) of
          Just item -> modify (useItem item) -- Use the selected item
          Nothing   -> modify (\s -> s { Game.message = "Invalid selection." : Game.message s })
        modify (\s -> s { Game.commandMode = False }) -- Exit command mode after using an item
      KEsc -> modify (\s -> s { Game.commandMode = False }) -- Exit command mode without selecting
      _ -> return () -- No-op for other keys
    else case key of
      KChar c   -> modify (\s -> s { Game.commandBuffer = Game.commandBuffer s ++ [c] })     -- Add to command buffer
      KBS       -> modify (\s -> s { Game.commandBuffer = initSafe (Game.commandBuffer s) }) -- Backspace
      KEnter    -> do
        cmd <- gets Game.commandBuffer
        executeCommand cmd
        modify (\s -> s { Game.commandMode = False, Game.commandBuffer = "" }) -- Exit command mode after execution
      KEsc      -> modify (\s -> s { Game.commandMode = False, Game.commandBuffer = "" }) -- Exit command mode without executing
      _         -> return () -- No-op for other keys

-- Execute commands
executeCommand :: String -> EventM () Game.GameState ()
executeCommand ":q" = halt -- Quit the game
executeCommand cmd  = modify (\s -> s { Game.message = ("Unknown command: " ++ cmd) : Game.message s })

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
      monsterAt pos = filter (\m -> Game.mPosition m == pos) (Game.monsters currentWorld)
      canMove pos =
        y >= 0 && y < length worldMap &&
        x >= 0 && x < length (head worldMap) &&
        (worldMap !! y !! x) /= Game.Wall
        where
          V2 x y = pos
  in case monsterAt newPos of
       [] -> if canMove newPos
             then
               let updatedWorld = updateVisibility (Game.player state) 5 currentWorld
               in state { Game.player = (Game.player state) { Game.position = newPos }
                        , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }
             else state
       (monster:_) -> combat state monster newPos

-- Player hits a monster and the monster returns the favor
combat :: Game.GameState -> Game.Monster -> V2 Int -> Game.GameState
combat state mnstr _ =
  let player = Game.player state
      playerDamage = Game.attack player
      monsterDamage = max 0 (defaultMonsterAttack - Game.resistance player)
      newHealth = max 0 (Game.health player - monsterDamage)
      updatedPlayer = player { Game.health = newHealth }
      currentWorld = Game.levels state !! Game.currentLevel state
      updatedMonsters =
        if Game.mHealth mnstr - playerDamage <= 0
        then filter (/= mnstr) (Game.monsters currentWorld)
        else map (\m -> if m == mnstr then m { Game.mHealth = Game.mHealth mnstr - playerDamage } else m)
                 (Game.monsters currentWorld)
      updatedWorld = currentWorld { Game.monsters = updatedMonsters }
      isDead = newHealth == 0
      newMessage = if isDead
                   then "You have died! Game Over." : Game.message state
                   else ("The " ++ Game.mName mnstr ++ " attacked you for " ++ show monsterDamage ++ " damage!") :
                        ("You attacked " ++ Game.mName mnstr ++ " for " ++ show playerDamage ++ " damage!") :
                        Game.message state
  in state { Game.player = updatedPlayer
           , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld
           , Game.message = newMessage
           , Game.gameOver = isDead }

-- Move monsters in the current level
moveMonsters :: Game.GameState -> Game.GameState
moveMonsters state =
  let currentWorld = Game.levels state !! Game.currentLevel state
      playerPos = Game.position (Game.player state)
      updatedMonsters = map (moveMonster currentWorld playerPos) (Game.monsters currentWorld)
      updatedWorld = currentWorld { Game.monsters = updatedMonsters }
  in state { Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld }

-- Move a single monster towards the player if within range
moveMonster :: Game.World -> V2 Int -> Game.Monster -> Game.Monster
moveMonster world playerPos monster =
  let monsterPos = Game.mPosition monster
      distance = manhattanDistance playerPos monsterPos
      potentialMoves = filter (isValidMove world) [V2 (x+dx) (y+dy) | (dx, dy) <- moveDirections]
        where V2 x y = monsterPos
      moveDirections =
        if distance <= 4
        then prioritizeTowardsPlayer playerPos monsterPos
        else [(0, 0)] -- Stay in place if out of range
  in case potentialMoves of
       (newPos:_) -> monster { Game.mPosition = newPos } -- Move to the first valid position
       _          -> monster -- Stay in place if no valid moves

-- Check if a position is valid for monster movement
isValidMove :: Game.World -> V2 Int -> Bool
isValidMove world pos =
  let V2 x y = pos
      grid = Game.mapGrid world
  in y >= 0 && y < length grid &&
     x >= 0 && x < length (head grid) &&
     (grid !! y !! x) /= Game.Wall -- Ensure not a wall

-- Prioritize movement directions towards the player
prioritizeTowardsPlayer :: V2 Int -> V2 Int -> [(Int, Int)]
prioritizeTowardsPlayer (V2 px py) (V2 mx my) =
  let dx = signum (px - mx) -- Direction in x-axis
      dy = signum (py - my) -- Direction in y-axis
  in [(dx, 0), (0, dy), (dx, dy), (dx, -dy), (-dx, dy)] -- Prioritize direct and then diagonal moves

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap defAttr
  [ (attrName "fog", withBackColor defAttr black)
  , (attrName "discovered", withBackColor defAttr (rgbColor (40 :: Int) 40 40)) -- Dimly lit
  , (attrName "wall", withForeColor (withBackColor defAttr black) white)
  , (attrName "floor", withBackColor defAttr white)
  , (attrName "door", withForeColor defAttr yellow)
  , (attrName "upStair", withForeColor defAttr green)
  , (attrName "downStair", withForeColor defAttr green)
  , (attrName "player", withForeColor defAttr blue)
  , (attrName "monster", withForeColor defAttr red)
  , (attrName "item", withForeColor defAttr magenta)
  , (attrName "log", withForeColor defAttr yellow)
  ]
