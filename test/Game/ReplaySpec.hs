-- test/Game/ReplaySpec.hs
--
-- A run written down has to be the run it was. The bot plays one and hands
-- back the keys it pressed, which is a recorded run with no keyboard in it.
module Game.ReplaySpec (spec) where

import File.MapIO (loadNewGame)
import Game.Autoplay (stepOnce)
import Game.Replay
import Game.Score (runOf, runScore)
import Game.State (newGame)
import Game.Types
import Control.Monad (void)
import System.Directory (doesFileExist)
import System.Random (mkStdGen)
import Test.Hspec

import Fixtures

-- Play the dungeon and keep every key, which is what the keyboard would.
recordRunOf :: Int -> IO (Replay, GameState)
recordRunOf seed = do
  config <- loadNewGame >>= shouldSucceed
  start <- shouldSucceed (newGame (mkStdGen seed) config)
  let go n s
        | gameOver s || gameWon s || n > (20000 :: Int) = s
        | otherwise = case stepOnce s of
            Nothing -> s
            Just (keys, s') -> go (n + 1) s' {keysPressed = reverse keys ++ keysPressed s'}
      ended = go 0 start
  pure (recordOf (digestOf "a dungeon") seed ended, ended)

spec :: Spec
spec = do
  describe "the fingerprint of a dungeon" $ do
    it "is the same for the same bytes" $
      digestOf "world" `shouldBe` digestOf "world"

    it "differs when a single character does" $
      digestOf "world" `shouldNotBe` digestOf "worle"

    it "is a fixed width, whatever it is given" $
      map (length . digestOf) ["", "a", concat (replicate 500 "abc")]
        `shouldBe` [16, 16, 16]

  describe "playing a run back" $ do
    let config = loadNewGame >>= shouldSucceed
        dungeon = digestOf "a dungeon"

    it "refuses a run recorded against another dungeon" $ do
      cfg <- config
      let rec = Replay {replayWorld = "0000000000000000", replaySeed = 1,
                        replayKeys = "", replayRun = Nothing}
      case replay dungeon cfg rec of
        Left (WrongDungeon written given) -> (written, given) `shouldBe` ("0000000000000000", dungeon)
        other -> expectationFailure ("expected a refusal, got " ++ show (void other))

    it "lands the player exactly where the keys left them" $ do
      cfg <- config
      (rec, ended) <- recordRunOf 1
      case replay dungeon cfg rec of
        Left d -> expectationFailure ("diverged: " ++ show d)
        Right again -> do
          position (player again) `shouldBe` position (player ended)
          health (player again) `shouldBe` health (player ended)
          currentLevel again `shouldBe` currentLevel ended

    -- The point of the whole thing: the score a run claims is one anybody
    -- with the dungeon can check for themselves.
    it "comes to the same score it was recorded with" $ do
      cfg <- config
      (rec, ended) <- recordRunOf 1
      case replay dungeon cfg rec of
        Left d -> expectationFailure ("diverged: " ++ show d)
        Right again ->
          fmap runScore (runOf "replay" again) `shouldBe` fmap runScore (runOf "replay" ended)

    it "notices a run that claims an ending it did not have" $ do
      cfg <- config
      (rec, _) <- recordRunOf 1
      let lying = rec {replayRun = fmap (\r -> r {runTreasure = 999999}) (replayRun rec)}
      case replay dungeon cfg lying of
        Left (EndedDifferently written got) -> runTreasure written `shouldNotBe` runTreasure got
        other -> expectationFailure ("expected a mismatch, got " ++ show (void other))

    it "keeps the keys in the order they were pressed" $ do
      (rec, _) <- recordRunOf 1
      take 3 (replayKeys rec) `shouldNotBe` ""

  describe "the world it was played on" $ do
    hasWorld <- runIO (doesFileExist "world.json")
    if not hasWorld
      then it "requires world.json" $ pendingWith "run the test-suite from the package root"
      else it "replays the shipped dungeon to the same ending, twice over" $ do
        cfg <- loadNewGame >>= shouldSucceed
        (rec, _) <- recordRunOf 2
        let once = replay (digestOf "a dungeon") cfg rec
            twice = replay (digestOf "a dungeon") cfg rec
        fmap (fmap runScore . runOf "replay") once
          `shouldBe` fmap (fmap runScore . runOf "replay") twice
