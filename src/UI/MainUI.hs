-- src/UI/MainUI.hs
{-# OPTIONS_GHC -Wno-x-partial #-}

module UI.MainUI (startGame) where

import Brick
import Graphics.Vty (Event(..), Key(..), defAttr, black, white, yellow, green, red, blue, magenta)
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import Game.State (initGame, defaultHealth, defaultMonsterAttack)
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
handleMovement key =
  case key of
    KChar 'w' -> modify (movePlayer Game.North)   -- Move North
    KChar 's' -> modify (movePlayer Game.South)   -- Move South
    KChar 'a' -> modify (movePlayer Game.West)    -- Move West
    KChar 'd' -> modify (movePlayer Game.East)    -- Move East
    KChar '<' -> modify goUp                      -- Go up stairs
    KChar '>' -> modify goDown                    -- Go down stairs
    KChar 'g' -> modify pickUpItem                -- Pick up item
    KChar 'u' -> modify promptUseItem             -- Use item prompt
    KChar ':' -> modify (\s -> s { Game.commandMode = True, Game.commandBuffer = ":" }) -- Enter command mode
    _         -> return ()                        -- No-op for other keys

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
           in state { Game.currentLevel = newLevel
                    , Game.player = (Game.player state) { Game.position = playerPos }
                    , Game.message = "You ascend the stairs." : Game.message state
                    }
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
           in state { Game.currentLevel = newLevel
                    , Game.player = (Game.player state) { Game.position = playerPos }
                    , Game.message = "You descend the stairs." : Game.message state
                    }
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
  where
    replaceLevel ste levelIndex newWorld =
      take levelIndex (Game.levels ste) ++ [newWorld] ++ drop (levelIndex + 1) (Game.levels ste)

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
          state { Game.player = plyr { Game.equippedWeapon = Just itm
                                     , Game.attack = Game.attack plyr + Game.iEffectValue itm
                                     }
                , Game.message = ("You equipped " ++ Game.iName itm ++ ".") : Game.message state }
        Game.Armor  ->
          state { Game.player = plyr { Game.equippedArmor = Just itm
                                     , Game.resistance = Game.resistance plyr + Game.iEffectValue itm
                                     }
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
      worldMap  = Game.mapGrid currentWorld
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
             then state { Game.player = (Game.player state) { Game.position = newPos }
                        , Game.message = ("Moved to: " ++ show newPos) : Game.message state }
             else state { Game.message = ("Blocked at: " ++ show newPos) : Game.message state }
       (monster:_) ->
         combat state monster newPos

-- Player hits a monster and the monster returns the favor
combat :: Game.GameState -> Game.Monster -> V2 Int -> Game.GameState
combat state mnstr _ =
  let player = Game.player state
      playerDamage = Game.attack player
      monsterDamage = max 0 (defaultMonsterAttack - Game.resistance player) -- Monster attack is reduced by player's resistance
      playerHealth = Game.health player - monsterDamage
      monsterHealth = Game.mHealth mnstr - playerDamage
      updatedPlayer = player { Game.health = playerHealth }
      currentWorld = Game.levels state !! Game.currentLevel state
      updatedMonsters =
        if monsterHealth <= 0
        then filter (\m -> m /= mnstr) (Game.monsters currentWorld)
        else map (\m -> if m == mnstr then m { Game.mHealth = monsterHealth } else m) (Game.monsters currentWorld)
      updatedWorld = currentWorld { Game.monsters = updatedMonsters }
  in state { Game.player = updatedPlayer
           , Game.levels = replaceLevel state (Game.currentLevel state) updatedWorld
           , Game.message =
               if monsterHealth <= 0
               then ("You defeated " ++ Game.mName mnstr ++ "!") : Game.message state
               else ("You attacked " ++ Game.mName mnstr ++ "! It has " ++ show monsterHealth ++ " HP left.") :
                    ("The " ++ Game.mName mnstr ++ " attacked you! You have " ++ show playerHealth ++ " HP left.") :
                    Game.message state
           }
  where
    replaceLevel ste levelIndex newWorld =
      take levelIndex (Game.levels ste) ++ [newWorld] ++ drop (levelIndex + 1) (Game.levels ste)

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap defAttr
  [ (attrName "wall", bg black)
  , (attrName "floor", bg white)
  , (attrName "door", bg yellow)
  , (attrName "upStair", bg green)
  , (attrName "downStair", bg red)
  , (attrName "player", fg blue)
  , (attrName "monster", fg red)
  , (attrName "item", fg magenta)
  , (attrName "log", fg yellow)
  ]
