-- src/UI/Draw.hs
module UI.Draw (drawUI) where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Game.Types
import Linear.V2 (V2(..))

-- Draw the UI
drawUI :: GameState -> [Widget ()]
drawUI state =
  [ vBox
      [ drawTitleBar
      , hBox
          [ padRight (Pad 2) $ drawMap currentWorld (player state)
          , padLeft (Pad 2) $ drawLegend
          ]
      , padTop (Pad 2) $ drawMessages updatedMessages
      , padTop (Pad 1) $ drawCommandInput state
      ]
  ]
  where
    currentWorld = levels state !! currentLevel state
    playerPos = position (player state)
    itemsOnPlayerTile = [iName item | item <- items currentWorld, iPosition item == playerPos]
    currentTileMessage =
      if null itemsOnPlayerTile
        then "Nothing here."
        else "You see: " ++ unwords itemsOnPlayerTile

    -- Combine the tile-specific message with the general log
    updatedMessages = currentTileMessage : message state

drawTitleBar :: Widget ()
drawTitleBar =
      padBottom (Pad 1) $ C.hCenter (str "Rogue-like Game")

-- Draw the map
drawMap :: World -> Player -> Widget ()
drawMap wrld plyr =
  B.border $
    vBox $ zipWith drawRow [0..] (mapGrid wrld)
  where
    mnstrs = monsters wrld
    itms = items wrld

    drawRow y row =
      hBox $ zipWith (\x tile -> drawTileWithEntities wrld plyr mnstrs itms x y tile) [0..] row

drawTileWithEntities :: World -> Player -> [Monster] -> [Item] -> Int -> Int -> Tile -> Widget ()
drawTileWithEntities _ plyr mnstrs itms x y tile
  | playerPos == V2 x y = withAttr (attrName "player") $ str "@" -- Player
  | monsterAt (V2 x y) = withAttr (attrName "monster") $ str "M"  -- Monster
  | itemAt (V2 x y) = withAttr (attrName "item") $ str "!"       -- Item
  | otherwise = drawTile tile
  where
    playerPos = position plyr
    monsterAt pos = any ((== pos) . mPosition) mnstrs
    itemAt pos = any ((== pos) . iPosition) itms

-- Draw a single tile
drawTile :: Tile -> Widget ()
drawTile Wall      = str "#"
drawTile Floor     = str "."
drawTile Door      = str "+"
drawTile UpStair   = str "<"
drawTile DownStair = str ">"

-- Draw the legend
drawLegend :: Widget ()
drawLegend =
  vBox $ map str
    [ "Commands:"
    , "w - Move up"
    , "s - Move down"
    , "a - Move left"
    , "d - Move right"
    , "< - Ascend stairs/ladder"
    , "> - Descend stairs/ladder"
    , ": - Enter command mode"
    , ":q - Quit the game"
    ]

-- Draw messages/log
drawMessages :: [String] -> Widget ()
drawMessages msgs =
      vLimit 3 $ -- Limit to 3 rows
        vBox $ map str (reverse . take 3 $ msgs)

-- Draw the command input bar
drawCommandInput :: GameState -> Widget ()
drawCommandInput state =
  withBorderStyle BS.unicodeBold $
    hBox
      [ str "Command: "
      , showCursor () (Location (length (commandBuffer state), 0)) $
          str (commandBuffer state ++ " ")
      ]
