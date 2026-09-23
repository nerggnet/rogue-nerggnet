-- test/File/MapIOSpec.hs
--
-- These specs exercise the real world.json, so they need the test-suite to run
-- with the package root as the working directory (which is what cabal does).
module File.MapIOSpec (spec) where

import Control.Exception (evaluate, finally)
import Data.Aeson (Result (..), Value (Object), fromJSON, toJSON)
import Data.List (nub)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Control.Monad (when)
import File.Paths
import Game.Autoplay (autoplay, outcome, Outcome (..))
import File.MapIO (deleteSave, loadNewGame, loadSavedGame, persistGame, saveGame)
import Game.Logic (executeAction, rollDamage)
import Game.GridUtils (gridLookup)
import Game.State (currentWorld, newGame)
import Game.Types
import Linear.V2 (V2 (..))
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import File.Scores
import Test.Hspec

import Fixtures (shouldSucceed, testGen)

-- | Run an action with a scratch save file, removing it afterwards.
withTempSave :: (FilePath -> IO a) -> IO a
withTempSave act = do
  dir <- getTemporaryDirectory
  let path = dir </> "rogue-nerggnet-spec-save.json"
  act path `finally` do
    exists <- doesFileExist path
    when exists (removeFile path)

-- | A fresh game built from the repository's world.json.
freshGame :: IO GameState
freshGame = loadNewGame defaultWorldFile >>= \config -> shouldSucceed (config >>= newGame testGen)

-- Put a corpse on the current level, as combat would.
withCorpse :: GameState -> GameState
withCorpse state =
  state {levels = map (\w -> w {corpses = [V2 3 1]}) (levels state)}

isPosAndItems :: TriggerCondition -> Bool
isPosAndItems (AtPositionWithItems _ _) = True
isPosAndItems _ = False

-- | Save a state and read it back the way startGame does.
roundTrip :: GameState -> IO GameState
roundTrip state =
  withTempSave $ \path -> do
    saveGame path state
    loaded <- loadSavedGame defaultWorldFile path
    case loaded of
      Left problems -> fail ("loadSavedGame failed: " ++ unlines problems)
      Right s  -> pure s

spec :: Spec
spec = do
  scoresSpec
  exampleSpec
  effectsSpec
  hasWorld <- runIO (doesFileExist "world.json")
  if not hasWorld
    then it "requires world.json" $
           pendingWith "run the test-suite from the package root"
    else describe "world.json" $ do
      it "loads as a fresh game" $ do
        state <- freshGame
        length (levels state) `shouldSatisfy` (> 0)
        currentLevel state `shouldBe` 0

      it "starts the player on the S tile of the first level" $ do
        state <- freshGame
        let world = currentWorld state
            V2 x y = position (player state)
        (mapGrid world !! y !! x) `shouldBe` Start

      it "builds every trigger on every level without erroring" $ do
        state <- freshGame
        mapM_ (evaluate . triggerCondition) (concatMap triggers (levels state))

      it "builds the win condition on the first level" $ do
        state <- freshGame
        map triggerCondition (triggers (currentWorld state))
          `shouldSatisfy` any isPosAndItems

      it "gives the player the stats of the first XP level" $ do
        state <- freshGame
        playerXPLevel (player state) `shouldBe` 1
        health (player state) `shouldSatisfy` (> 0)

      -- A monster that hits softer a floor deeper than it did above reads
      -- as a descent in the file and plays as a reprieve, and nothing in
      -- the game says so: three of these were found only by measuring how
      -- much health a floor cost, one of them twice over. The rule is not
      -- that a name has to grow as the dungeon goes down, only that it must
      -- never shrink; where a floor fields several of a name, the weakest
      -- of them is what the next floor down has to match.
      it "never makes a monster weaker further down" $ do
        state <- freshGame
        let named = nub [mName m | world <- levels state, m <- monsters world]
            appearances n =
              [ (depth, minimum (map mAttack ms), minimum (map mHealth ms))
              | (depth, world) <- zip [1 :: Int ..] (levels state)
              , let ms = [m | m <- monsters world, mName m == n]
              , not (null ms)
              ]
            weakening n =
              [ n ++ " is " ++ stats a1 h1 ++ " on floor " ++ show d1
                  ++ " but " ++ stats a2 h2 ++ " on floor " ++ show d2
              | ((d1, a1, h1), (d2, a2, h2)) <- zip seen (drop 1 seen)
              , a2 < a1 || h2 < h1
              ]
              where seen = appearances n
            stats a h = show a ++ "/" ++ show h
        concatMap weakening named `shouldBe` []

      describe "loading an older save" $
        it "fills in on-screen fields the save does not have" $ do
          state <- freshGame
          let transient =
                [ "legendPage", "commandBuffer", "commandMode", "commandToExecute"
                , "inventoryMode", "keyPressCount", "lastInteractedNpc"
                , "aimingState", "gameOver", "gameWon", "message"
                ]
          slim <- case toJSON state of
            Object o -> pure (Object (foldr (KM.delete . Key.fromString) o transient))
            other -> other <$ expectationFailure "a game state should encode as an object"
          case fromJSON slim of
            Error err -> expectationFailure err
            Success reloaded -> do
              legendPage reloaded `shouldBe` 0
              commandMode reloaded `shouldBe` False
              gameOver reloaded `shouldBe` False
              message reloaded `shouldBe` []
              -- and the durable half survived
              position (player reloaded) `shouldBe` position (player state)
              length (levels reloaded) `shouldBe` length (levels state)

      describe "persistGame" $ do
        it "writes a save while the run is still going" $
          withTempSave $ \path -> do
            state <- freshGame
            persistGame path state
            doesFileExist path `shouldReturn` True

        it "removes the save when the player has died" $
          withTempSave $ \path -> do
            state <- freshGame
            saveGame path state
            doesFileExist path `shouldReturn` True
            persistGame path state {gameOver = True}
            doesFileExist path `shouldReturn` False

        it "removes the save when the player has won" $
          withTempSave $ \path -> do
            state <- freshGame
            saveGame path state
            persistGame path state {gameWon = True}
            doesFileExist path `shouldReturn` False

        -- Dying on the very first run means there is nothing to delete.
        it "does not mind when there is no save to remove" $
          withTempSave $ \path -> do
            state <- freshGame
            doesFileExist path `shouldReturn` False
            persistGame path state {gameOver = True}
            doesFileExist path `shouldReturn` False

        it "leaves no save an old one could be resumed from" $
          withTempSave $ \path -> do
            state <- freshGame
            -- an earlier, healthy checkpoint
            persistGame path state
            doesFileExist path `shouldReturn` True
            -- the player then dies later in the same run
            persistGame path state {gameOver = True}
            doesFileExist path `shouldReturn` False

      describe "deleteSave" $ do
        it "removes an existing file" $
          withTempSave $ \path -> do
            writeFile path "{}"
            deleteSave path
            doesFileExist path `shouldReturn` False

        it "is a no-op for a file that is not there" $
          withTempSave $ \path -> deleteSave path

      describe "the save/load round trip" $ do
        it "preserves the player" $ do
          before' <- freshGame
          after' <- roundTrip before'
          position (player after') `shouldBe` position (player before')
          health (player after') `shouldBe` health (player before')
          xp (player after') `shouldBe` xp (player before')
          inventory (player after') `shouldBe` inventory (player before')

        it "preserves every level and its dimensions" $ do
          before' <- freshGame
          after' <- roundTrip before'
          length (levels after') `shouldBe` length (levels before')
          map mapRows (levels after') `shouldBe` map mapRows (levels before')
          map mapCols (levels after') `shouldBe` map mapCols (levels before')

        it "restores the map grid from world.json" $ do
          before' <- freshGame
          after' <- roundTrip before'
          map mapGrid (levels after') `shouldBe` map mapGrid (levels before')

        it "preserves monsters, items, doors and NPCs" $ do
          before' <- freshGame
          after' <- roundTrip before'
          map monsters (levels after') `shouldBe` map monsters (levels before')
          map items (levels after') `shouldBe` map items (levels before')
          map doors (levels after') `shouldBe` map doors (levels before')
          map npcs (levels after') `shouldBe` map npcs (levels before')

        it "preserves every trigger exactly" $ do
          before' <- freshGame
          after' <- roundTrip before'
          map triggers (levels after') `shouldBe` map triggers (levels before')

        it "preserves discovered tiles" $ do
          before' <- freshGame
          after' <- roundTrip before'
          let discoveredCount = length . concatMap (filter id) . discovered
          map discoveredCount (levels after')
            `shouldBe` map discoveredCount (levels before')

        it "carries the generator on, rather than restarting it" $ do
          before' <- freshGame
          after' <- roundTrip before'
          -- The reloaded game must roll what the saved one was about to.
          fst (rollDamage 100 (rng after')) `shouldBe` fst (rollDamage 100 (rng before'))

        it "preserves corpses" $ do
          before' <- freshGame
          let fought = withCorpse before'
          after' <- roundTrip fought
          corpses (currentWorld after') `shouldBe` [V2 3 1]

        it "re-applies tile overrides left by shiftTile" $ do
          before' <- freshGame
          let shifted = executeAction before' (ShiftTile (V2 0 1) Floor)
          after' <- roundTrip shifted
          let world = currentWorld after'
          gridLookup (mapGrid world) (V2 0 1) `shouldBe` Just Floor
          tileOverrides world `shouldBe` [(V2 0 1, Floor)]

-- | Run an action with a scratch scoreboard, removing it afterwards.
withTempScores :: (FilePath -> IO a) -> IO a
withTempScores act = do
  dir <- getTemporaryDirectory
  let path = dir </> "rogue-nerggnet-spec-scores.json"
  act path `finally` do
    exists <- doesFileExist path
    when exists (removeFile path)

-- An effect with no item is a feature the game has and nobody can reach.
-- Blink, Lifesteal and Revive were all three implemented, specced and
-- written up in the README, and for a long time no item in the dungeon had
-- any of them.
effectsSpec :: Spec
effectsSpec = describe "the effects the dungeon uses" $
  it "puts every effect the game implements on some item" $ do
    game <- freshGame
    let placed = [e | w <- levels game, i <- items w, Just e <- [iEffect i]]
    mapM_ (\e -> (e, e `elem` placed) `shouldBe` (e, True))
          ([minBound .. maxBound] :: [ItemEffect])

-- The dungeon that ships is not the only one the engine can play, and an
-- example that does not load is worse than no example.
exampleSpec :: Spec
exampleSpec = describe "the example dungeon" $ do
  hasIt <- runIO (doesFileExist "example.json")
  if not hasIt
    then it "requires example.json" $ pendingWith "run the test-suite from the package root"
    else do
      it "loads and builds a game" $ do
        config <- loadNewGame "example.json"
        st <- shouldSucceed (config >>= newGame testGen)
        length (levels st) `shouldBe` 1

      it "can be finished" $ do
        config <- loadNewGame "example.json"
        st <- shouldSucceed (config >>= newGame testGen)
        outcome (autoplay 4000 st) `shouldBe` Escaped

      -- Two dungeons must not share a scoreboard.
      it "keeps its own files, away from the shipped dungeon's" $
        scoresFile (pathsFor "example.json") `shouldNotBe` scoresFile (pathsFor defaultWorldFile)

scoresSpec :: Spec
scoresSpec = describe "the scoreboard file" $ do
  let finished st = st {gameWon = True, deepestLevel = 2, turnCount = 40}

  it "is empty when there is no file yet" $
    withTempScores $ \path ->
      loadScores path `shouldReturn` Right []

  it "writes a finished run and reads it back" $
    withTempScores $ \path -> do
      game <- freshGame
      _ <- recordRun path "2026-01-01 00:00" (finished game)
      board <- loadScores path
      fmap (map runDepth) board `shouldBe` Right [3]

  it "keeps the runs already recorded" $
    withTempScores $ \path -> do
      game <- freshGame
      _ <- recordRun path "2026-01-01 00:00" (finished game)
      _ <- recordRun path "2026-01-02 00:00" (finished game)
      board <- loadScores path
      fmap length board `shouldBe` Right 2

  it "says where the run placed" $
    withTempScores $ \path -> do
      game <- freshGame
      let rich = (finished game) {deepestLevel = 9}
      _ <- recordRun path "2026-01-01 00:00" (finished game)
      placed <- recordRun path "2026-01-02 00:00" rich
      fmap (\(_, place, outOf) -> (place, outOf)) placed `shouldBe` Just (1, 2)

  it "records nothing for a game still being played" $
    withTempScores $ \path -> do
      game <- freshGame
      recordRun path "2026-01-01 00:00" game `shouldReturn` Nothing

  -- Losing the history is not a reason to refuse to play.
  it "reports a scoreboard it cannot parse, rather than throwing" $
    withTempScores $ \path -> do
      writeFile path "this is not json"
      board <- loadScores path
      board `shouldSatisfy` either (const True) (const False)
