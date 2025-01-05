-- src/UI/MainUI.hs
{-# OPTIONS_GHC -Wno-x-partial #-}

module UI.MainUI (startGame) where

import Brick
import Graphics.Vty (Event(..), Key(..), defAttr, black, white, yellow, green, red)
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import Game.State (initGame)
import UI.Draw
import qualified Game.Types as Game
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))

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
  _ <- customMain vty buildVty Nothing app initialState -- end state
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
    KChar ':' -> modify (\s -> s { Game.commandMode = True, Game.commandBuffer = "" }) -- Enter command mode
    _         -> return ()                        -- No-op for other keys

-- Go up stairs
goUp :: Game.GameState -> Game.GameState
goUp state =
  let playerPos = Game.position (Game.player state)
      tile = (Game.mapGrid (Game.world state)) !! (playerPos ^. _y) !! (playerPos ^. _x)
  in case tile of
       Game.UpStair -> -- Logic to transition to the upper level
         state { Game.message = "You ascend the stairs." : Game.message state }
       _ -> state { Game.message = "No stairs to go up here!" : Game.message state }

-- Go down stairs
goDown :: Game.GameState -> Game.GameState
goDown state =
  let playerPos = Game.position (Game.player state)
      tile = (Game.mapGrid (Game.world state)) !! (playerPos ^. _y) !! (playerPos ^. _x)
  in case tile of
       Game.DownStair -> -- Logic to transition to the lower level
         state { Game.message = "You descend the stairs." : Game.message state }
       _ -> state { Game.message = "No stairs to go down here!" : Game.message state }

-- Handle commands
handleCommandInput :: Key -> EventM () Game.GameState ()
handleCommandInput key =
  case key of
    KChar c   -> modify (\s -> s { Game.commandBuffer = Game.commandBuffer s ++ [c] })     -- Add to command buffer
    KBS       -> modify (\s -> s { Game.commandBuffer = initSafe (Game.commandBuffer s) }) -- Backspace
    KEnter    -> do
      cmd <- gets Game.commandBuffer
      executeCommand cmd
      modify (\s -> s { Game.commandMode = False, Game.commandBuffer = "" })            -- Exit command mode after execution
    KEsc      -> modify (\s -> s { Game.commandMode = False, Game.commandBuffer = "" }) -- Exit command mode without executing
    _         -> return ()                                                              -- No-op for other keys

-- Execute commands
executeCommand :: String -> EventM () Game.GameState ()
executeCommand "q" = halt -- Quit the game
executeCommand cmd  = modify (\s -> s { Game.message = ("Unknown command: " ++ cmd) : Game.message s })

-- Safe init for empty lists
initSafe :: [a] -> [a]
initSafe [] = []
initSafe xs = init xs

-- Move the player in a direction
movePlayer :: Game.Direction -> Game.GameState -> Game.GameState
movePlayer dir state =
  let playerPos = Game.position (Game.player state)
      worldMap  = Game.mapGrid (Game.world state)
      newPos = case dir of
        Game.North -> playerPos + V2 0 (-1)
        Game.South -> playerPos + V2 0 1
        Game.West  -> playerPos + V2 (-1) 0
        Game.East  -> playerPos + V2 1 0
        _          -> playerPos
      canMove (V2 x y) =
        y >= 0 && y < length worldMap &&
        x >= 0 && x < length (head worldMap) &&
        (worldMap !! y !! x) /= Game.Wall
      canMoveTo = canMove newPos
  in if canMoveTo
        then state { Game.player = (Game.player state) { Game.position = newPos }
                   , Game.message = ("Moved to: " ++ show newPos) : Game.message state
                   }
        else state { Game.message = ("Blocked at: " ++ show newPos) : Game.message state }

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap defAttr
  [ (attrName "wall", bg black)
  , (attrName "floor", bg white)
  , (attrName "door", bg yellow)
  , (attrName "upStair", bg green)
  , (attrName "downStair", bg red)
  ]
