-- test/Game/ScoreSpec.hs
--
-- What a finished run is worth, and the order they go in.
module Game.ScoreSpec (spec) where

import Game.Score
import Game.Types
import Test.Hspec

import Fixtures

-- A run with everything at zero, to be filled in by record update.
blank :: Run
blank = Run
  { runWhen = "2026-01-01 00:00", runEnding = GotOut
  , runDepth = 1, runTreasure = 0, runXP = 0, runTurns = 0
  }

spec :: Spec
spec = do
  describe "what a run is worth" $ do
    it "counts the treasure that came out" $
      runScore blank {runTreasure = 500} `shouldBe` 500 + 100

    it "pays for depth as well, so a deep death beats a shallow one" $
      runScore blank {runEnding = Killed, runDepth = 11}
        `shouldSatisfy` (> runScore blank {runEnding = Killed, runDepth = 2})

  describe "recording a run" $ do
    let finished won st = st {gameOver = not won, gameWon = won, deepestLevel = 4}
        carrying = withPlayer (\p -> p {inventory = [mkTreasure "Crown" 900], xp = 77})

    it "records nothing while the game is still going" $
      runOf "now" baseState `shouldBe` Nothing

    it "counts the treasure of a run that got out" $
      fmap runTreasure (runOf "now" (finished True (carrying baseState)))
        `shouldBe` Just 900

    -- Getting out is what turns treasure carried into treasure kept.
    it "brings nothing out of a run that ended badly" $
      fmap runTreasure (runOf "now" (finished False (carrying baseState)))
        `shouldBe` Just 0

    it "still credits the depth of a run that ended badly" $
      fmap runDepth (runOf "now" (finished False baseState)) `shouldBe` Just 5

    it "says how it ended" $ do
      fmap runEnding (runOf "now" (finished True baseState)) `shouldBe` Just GotOut
      fmap runEnding (runOf "now" (finished False baseState)) `shouldBe` Just Killed

    it "keeps the turns the run lasted" $
      fmap runTurns (runOf "now" (finished True baseState) {turnCount = 812})
        `shouldBe` Just 812

  describe "the order they go in" $ do
    let rich = blank {runTreasure = 900, runTurns = 500}
        poor = blank {runTreasure = 100, runTurns = 20}
        brisk = blank {runTreasure = 900, runTurns = 300}

    it "puts the better score first" $
      map runTreasure (ranked [poor, rich]) `shouldBe` [900, 100]

    -- Two identical hauls are separated by who wasted less time getting them.
    it "breaks a tie on the shorter run" $
      map runTurns (ranked [rich, brisk]) `shouldBe` [300, 500]

    it "is stable enough to place a run in it" $ do
      placeOf brisk [rich, poor, brisk] `shouldBe` Just 1
      placeOf rich [rich, poor, brisk] `shouldBe` Just 2
      placeOf poor [rich, poor, brisk] `shouldBe` Just 3

    it "cannot place a run that is not in the table" $
      placeOf blank [rich, poor] `shouldBe` Nothing
