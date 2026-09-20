-- test/UI/LayoutSpec.hs
--
-- The screen has to fit the terminal. Anything that does not fit is silently
-- cut off the bottom, and the bottom is where the newest log line and the
-- command prompt live, so a layout that is too tall looks like the game
-- losing messages rather than like a layout problem.
module UI.LayoutSpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf, isPrefixOf)
import Game.State (defaultFogRadius, updateVisibility, withCurrentWorld)
import Game.Types
import Linear.V2 (V2 (..))
import Test.Hspec
import UI.Draw (drawUI)

import Fixtures
import Render

-- A dungeon the size of the ones in world.json: 27 rows of 51 columns.
bigMap :: [String]
bigMap =
  [replicate 51 '#']
    ++ replicate 25 ("#" ++ replicate 49 '.' ++ "#")
    ++ [replicate 51 '#']

-- A log at capacity, which is when the screen is at its tallest.
fullLog :: [String]
fullLog = ["NEWEST", "older-1", "older-2", "older-3", "older-4", "older-5"]

-- Light the area around the player, as starting a game does. Without this
-- the whole map renders as unexplored fog.
lit :: GameState -> GameState
lit st = withCurrentWorld (updateVisibility (player st) defaultFogRadius) st

-- The player standing on an item, with a log at capacity.
onItem :: GameState
onItem =
  lit
    . withWorld (\w -> w {items = [mkItem "Ruby Amulet" Special 0 (V2 5 5)]})
    $ (mkState (mkWorld bigMap) (V2 5 5)) {message = fullLog}

-- Terminal sizes a player might plausibly be using. 80x24 is the classic
-- default; the taller ones are ordinary windows on a laptop.
sizes :: [(Int, Int)]
sizes = [(80, 24), (100, 30), (120, 36), (120, 40), (150, 50)]

-- Every element equal, without assuming the list is non-empty.
allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (== x) xs

screen :: (Int, Int) -> GameState -> [String]
screen size st = renderRows size (drawUI st)

showsText :: (Int, Int) -> GameState -> String -> Bool
showsText size st needle = any (needle `isInfixOf`) (screen size st)

spec :: Spec
spec = describe "the game screen" $ do
  forM_ sizes $ \size@(w, h) ->
    describe (show w ++ "x" ++ show h) $ do
      it "shows the command prompt" $
        screen size onItem
          `shouldSatisfy` any (("Command:" `isPrefixOf`) . dropWhile (== ' '))

      it "shows the newest line of the log" $
        showsText size onItem "NEWEST" `shouldBe` True

      it "shows what the player is standing on" $
        showsText size onItem "Ruby Amulet" `shouldBe` True

      it "still draws the map" $
        showsText size onItem "@" `shouldBe` True

      it "shows the player's health" $
        showsText size onItem "HP:" `shouldBe` True

      it "never draws more rows than the terminal has" $
        length (screen size onItem) `shouldBe` h

  describe "the message pane" $ do
    let size = (120, 40)

    it "puts the newest line below the older ones" $ do
      let rows = screen size onItem
          rowOf needle = length (takeWhile (not . (needle `isInfixOf`)) rows)
      rowOf "NEWEST" `shouldSatisfy` (> rowOf "older-1")

    it "keeps showing the newest line as the log fills up" $
      forM_ [1 .. length fullLog] $ \n -> do
        let st = onItem {message = take n fullLog}
        showsText size st "NEWEST" `shouldBe` True

  describe "a steady layout" $ do
    -- The map takes whatever vertical space the rest of the screen leaves, so
    -- anything above it that changes size makes the map jump about while the
    -- player is reading it.
    let size = (120, 40)
        -- The map's border starts at column 0; the stats boxes are indented,
        -- so anchoring on column 0 measures the map and nothing else.
        mapHeight rows =
          let col0 c = [i | (i, r) <- zip [0 :: Int ..] rows, take 1 r == [c]]
           in case (col0 '\9484', col0 '\9492') of
                (t : _, b : _) -> b - t - 1
                _ -> error "no map border found"
        heightWith st = mapHeight (screen size st)

    it "keeps the map the same height however full the log is" $ do
      [heightWith onItem {message = take n fullLog} | n <- [0 .. length fullLog]]
        `shouldSatisfy` allSame

    it "keeps the map the same height however full the inventory is" $ do
      let stocked n =
            withPlayer (\pl -> pl {inventory = [mkItem ("Item" ++ show i) Special 0 (V2 0 0) | i <- [1 .. n]]}) onItem
      [heightWith (stocked n) | n <- [0, 5, 10, 15 :: Int]] `shouldSatisfy` allSame

    it "keeps the newest log line on the same row however full the log is" $ do
      let rowOfNewest n =
            let rows = screen size onItem {message = "NEWEST" : replicate n "older"}
             in length (takeWhile (not . ("NEWEST" `isInfixOf`)) rows)
      [rowOfNewest n | n <- [0 .. 5]] `shouldSatisfy` allSame

    it "keeps the map the same height when a message is very long" $ do
      let long = replicate 400 'x'
      heightWith onItem {message = [long]} `shouldBe` heightWith onItem {message = ["short"]}

  describe "scrolling" $ do
    -- A map bigger than the window has to move under the player rather than
    -- stretch the screen, or the rest of the layout goes off the bottom.
    let at p =
          withCurrentWorld (updateVisibility (mkPlayer p) defaultFogRadius)
            (mkState (mkWorld bigMap) p)
        corners = [V2 2 2, V2 48 2, V2 2 25, V2 48 25, V2 25 13]

    forM_ corners $ \p ->
      it ("keeps the player in view at " ++ show p) $
        screen (100, 30) (at p) `shouldSatisfy` any (elem '@')

    it "keeps the command prompt in view wherever the player is" $
      forM_ corners $ \p ->
        screen (100, 30) (at p)
          `shouldSatisfy` any (("Command:" `isPrefixOf`) . dropWhile (== ' '))

  describe "a large map" $ do
    -- The map must not be able to push the rest of the screen off the bottom,
    -- however big the dungeon is.
    let huge = replicate 80 (replicate 200 '.')
        st = lit ((mkState (mkWorld huge) (V2 1 1)) {message = fullLog})

    it "still leaves room for the command prompt" $
      screen (120, 40) st
        `shouldSatisfy` any (("Command:" `isPrefixOf`) . dropWhile (== ' '))

    it "still leaves room for the log" $
      showsText (120, 40) st "NEWEST" `shouldBe` True
