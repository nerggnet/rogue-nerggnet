-- test/Game/AutoplaySpec.hs
--
-- The dungeon is drawn by hand, so nothing about it guarantees it can be
-- finished. This plays it.
module Game.AutoplaySpec (spec) where

import File.MapIO (loadNewGame)
import Game.Autoplay
import Game.State
import Game.Types
import System.Directory (doesFileExist)
import System.Random (mkStdGen)
import Test.Hspec

import Fixtures

spec :: Spec
spec = do
  hasWorld <- runIO (doesFileExist "world.json")
  if not hasWorld
    then it "requires world.json" $ pendingWith "run the test-suite from the package root"
    else describe "the dungeon in world.json" $ do
      let played seed = do
            config <- loadNewGame
            start <- shouldSucceed (config >>= newGame (mkStdGen seed))
            pure (autoplay 12000 start, start)

      it "can be finished" $ do
        (run, _) <- played 1
        outcome run `shouldBe` Escaped

      it "goes all the way to the bottom" $ do
        (run, start) <- played 1
        deepestReached run `shouldBe` length (levels start)

      it "is worth finishing" $ do
        (run, _) <- played 1
        treasure run `shouldSatisfy` (> 5000)

      it "leaves the player alive at the end" $ do
        (run, _) <- played 1
        finalHealth run `shouldSatisfy` (> 0)

      -- Damage is rolled, so the same dungeon does not play out identically.
      -- It has to be beatable however the dice land, not just once.
      it "can be finished whichever way the dice fall" $
        mapM_
          (\seed -> do
              (run, _) <- played seed
              (seed, outcome run) `shouldBe` (seed, Escaped))
          [1 .. 6 :: Int]

      -- A dungeon nobody can lose is not a dungeon. The run has to be
      -- brought low somewhere along the way, and still come back.
      it "puts the player in real danger on the way" $ do
        (run, _) <- played 1
        lowestHealth run `shouldSatisfy` (< 60)

      it "is not so punishing that it is only survived by luck" $ do
        (run, _) <- played 1
        lowestHealth run `shouldSatisfy` (> 5)
