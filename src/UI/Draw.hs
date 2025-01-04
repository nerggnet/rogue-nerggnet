-- src/UI/Draw.hs
module UI.Draw (drawUI) where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Game.Types
import Linear.V2 (V2(..))
-- import Linear.V2 (V2(..), _x, _y)
-- import Control.Lens ((^.))
-- import Debug.Trace

-- Draw the UI
drawUI :: GameState -> [Widget ()]
drawUI state =
  [ vBox
      [ drawTitleBar
      , hBox
          [ padRight (Pad 2) $ drawMap (world state) (player state)
          , padLeft (Pad 2) $ drawLegend
          ]
      , padTop (Pad 2) $ drawMessages (message state)
      , padTop (Pad 1) $ drawCommandInput state
      ]
  ]

drawTitleBar :: Widget ()
drawTitleBar =
      padBottom (Pad 1) $ C.hCenter (str "Rogue-like Game")

-- Draw the map
drawMap :: World -> Player -> Widget ()
drawMap wrld plyr =
  B.border $
    vBox $ zipWith drawRow [0..] (mapGrid wrld)
  where
    drawRow y row =
      hBox $ zipWith (drawTileWithPlayer y) [0..] row

    drawTileWithPlayer y x tile
      | V2 x y == position plyr = str "@" -- Draw the player
      | otherwise                 = drawTile tile

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
