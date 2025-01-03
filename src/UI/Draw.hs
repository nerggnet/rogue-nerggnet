-- src/UI/Draw.hs
module UI.Draw (drawUI) where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Game.Types
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))


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

-- Draw the map centered around the player
drawMap :: World -> Player -> Widget ()
drawMap world player =
  B.border $
    vBox $ map drawRowWithPlayer (zip [0..] visibleMap)
  where
    visibleMap = extractVisibleMap (position player) (mapGrid world)

    drawRowWithPlayer (y, row) =
      hBox $ map (drawTileWithPlayer y) (zip [0..] row)

    drawTileWithPlayer y (x, tile)
      | V2 x y == centerOnGrid = str "@" -- Draw the player at the center
      | otherwise              = drawTile tile

    -- Player's position on the visible grid
    centerOnGrid = V2 25 13 -- Center of the 51x27 visible grid

-- Helper function to extract the visible portion of the map
extractVisibleMap :: V2 Int -> [[Tile]] -> [[Tile]]
extractVisibleMap (V2 px py) grid =
  let mapHeight = length grid
      mapWidth = if mapHeight > 0 then length (head grid) else 0
      topLeft = V2 (limitValue 0 (mapWidth - 51) (px - 25))
                   (limitValue 0 (mapHeight - 27) (py - 13))
      bottomRight = V2 (limitValue 0 (mapWidth - 1) (px + 25))
                       (limitValue 0 (mapHeight - 1) (py + 13))
      extractRow y = take 51 . drop (max 0 (topLeft ^. _x)) $ grid !! y
  in take 27 . drop (max 0 (topLeft ^. _y)) $
       [extractRow y | y <- [topLeft ^. _y .. bottomRight ^. _y]]

-- Helper to clamp a value within bounds
limitValue :: Int -> Int -> Int -> Int
limitValue minVal maxVal val = max minVal (min maxVal val)


-- Pad the map to ensure it's gridWidth x gridHeight
padMap :: Int -> Int -> Int -> Int -> [[Tile]] -> [[Tile]]
padMap gridWidth gridHeight radiusX radiusY visibleRows =
  let -- Pad each row horizontally
      padRow row = replicate (radiusX - length row `div` 2) Wall ++ row ++ replicate (radiusX - length row `div` 2) Wall
      paddedRows = map padRow visibleRows
      -- Pad rows vertically
      emptyRow = replicate gridWidth Wall
  in replicate (radiusY - length paddedRows `div` 2) emptyRow ++ paddedRows ++ replicate (radiusY - length paddedRows `div` 2) emptyRow

-- Safe indexing with padding for out-of-bounds areas
(!?) :: [[Tile]] -> (Int, Int) -> Tile
(!?) grid (y, x)
  | y < 0 || y >= length grid = Wall -- Default to Wall for out-of-bounds
  | x < 0 || x >= length (head grid) = Wall -- Default to Wall for out-of-bounds
  | otherwise = (grid !! y) !! x

-- Draw a single tile
drawTile :: Tile -> Widget ()
drawTile Wall      = str "#"
drawTile Floor     = str "."
drawTile Door      = str "+"
drawTile UpStair   = str "<"
drawTile DownStair = str ">"

-- Define tile attributes
tileAttr :: Tile -> AttrName
tileAttr Wall = attrName "wall"
tileAttr Floor = attrName "floor"
tileAttr Door = attrName "door"
tileAttr UpStair = attrName "upStair"
tileAttr DownStair = attrName "downStair"

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
