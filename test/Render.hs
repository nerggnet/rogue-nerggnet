-- test/Render.hs
--
-- Render the real widgets at a given terminal size and read back what each
-- row actually shows. Layout bugs (things pushed off the bottom of the
-- screen) are invisible to tests that only look at the pure state.
module Render (renderRows, terminal) where

import Brick.Main (renderWidget)
import Brick.Types (Widget)
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp (..))
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

-- | A terminal size, in (columns, rows).
terminal :: Int -> Int -> (Int, Int)
terminal cols rows = (cols, rows)

-- | What each row of the terminal shows, top to bottom.
renderRows :: (Int, Int) -> [Widget ()] -> [String]
renderRows region widgets =
  map rowText (V.toList (displayOpsForPic picture region))
  where
    picture = renderWidget Nothing widgets region
    rowText = concatMap opText . V.toList
    opText (TextSpan _ _ _ t) = TL.unpack t
    opText (Skip n) = replicate n ' '
    opText (RowEnd n) = replicate n ' '
