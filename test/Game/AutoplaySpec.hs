-- test/Game/AutoplaySpec.hs
--
-- The dungeon is drawn by hand, so nothing about it guarantees it can be
-- finished. This plays it.
module Game.AutoplaySpec (spec) where

import File.Paths (defaultWorldFile)
import File.MapIO (loadNewGame)
import Game.Autoplay
import Game.State
import Game.GridUtils (keyedInventory)
import Game.Types
import Linear.V2 (V2 (..))
import System.Directory (doesFileExist)
import System.Random (mkStdGen)
import Test.Hspec

import Fixtures

-- What the bot would press next, or nothing if it is out of ideas.
presses :: GameState -> Maybe [Char]
presses = fmap fst . stepOnce

-- A quiet room with the player in the middle of it, at the given health.
atHealth :: Int -> GameState
atHealth n = lit (withPlayer (\p -> p {health = n}) baseState)
  where lit = withCurrentWorld (updateVisibility (player baseState) defaultFogRadius)

carrying :: [Item] -> GameState -> GameState
carrying is = withPlayer (\p -> p {inventory = is})

-- The letter the chooser would offer for an item in the pack.
letterFor :: GameState -> Item -> Char
letterFor st i =
  case [k | (k, j) <- keyedInventory (inventory (player st))
                        (equippedWeapon (player st)) (equippedArmor (player st))
          , j == i] of
    (k : _) -> k
    [] -> '?' 

spec :: Spec
spec = do
  describe "what it does with what it is carrying" $ do
    -- A permanent gain is worth nothing in the pack, so there is no turn on
    -- which holding one beats having used it.
    it "uses an Empower charm the moment it has one" $ do
      let stone = mkSpecial "Whetstone" Empower 6
          st = carrying [stone] (atHealth 20)
      presses st `shouldBe` Just ['u', letterFor st stone]

    it "uses a Fortify charm too" $ do
      let charm = mkSpecial "Warding Charm" Fortify 8
          st = carrying [charm] (atHealth 20)
      presses st `shouldBe` Just ['u', letterFor st charm]

    it "leaves a keepsake alone; there is nothing to use it for" $ do
      let st = carrying [mkTreasure "Gold Coin" 90] (atHealth 20)
      presses st `shouldNotBe` Just ['u', 'a']

    -- A tool is kept for what it does, not weighed against the treasure
    -- competing for its slot, which it loses every time.
    it "will not shed a tool as dead weight" $ do
      let tools = [mkSpecial ("Charm" ++ show i) Vanish 5 | i <- [1 .. maxInventorySize :: Int]]
          crowded = carrying tools (atHealth 20)
      presses crowded `shouldNotSatisfy` maybe False (("x" ==) . take 1)

  describe "when it is losing" $ do
    let wolves n = withWorld (\w -> w {monsters = [mkMonster ("Wolf" ++ show i) p 40 9 | (i, p) <- zip [1 :: Int ..] (take n spots)]})
        spots = [V2 5 3, V2 3 3, V2 4 2, V2 4 4, V2 6 3]
        potion = (mkItem "Potion" Healing 50 (V2 0 0)) {iUses = Just 1}

    it "burns a crowd rather than trading blows with it" $ do
      let scroll = mkSpecial "Ashen Scroll" Firestorm 60
          st = wolves 3 (carrying [scroll] (atHealth 60))
      presses st `shouldBe` Just ['u', letterFor st scroll]

    it "saves the scroll for a crowd" $ do
      let scroll = mkSpecial "Ashen Scroll" Firestorm 60
          st = wolves 1 (carrying [scroll] (atHealth 60))
      presses st `shouldNotBe` Just ['u', letterFor st scroll]

    -- Drinking beats running while there is anything left to drink, and
    -- drinking is decided first, so the charm stays in the pack.
    it "drinks rather than running while it still can" $ do
      let phial = mkSpecial "Phial" Vanish 5
          st = wolves 1 (carrying [potion, phial] (atHealth 3))
      presses st `shouldBe` Just ['u', letterFor st potion]

    it "vanishes when it is nearly dead with nothing left to drink" $ do
      let phial = mkSpecial "Phial" Vanish 5
          st = wolves 1 (carrying [phial] (atHealth 3))
      presses st `shouldBe` Just ['u', letterFor st phial]

    it "blinks when there is no vanishing to be had" $ do
      let stone = mkSpecial "Waystone" Blink 0
          st = wolves 1 (carrying [stone] (atHealth 3))
      presses st `shouldBe` Just ['u', letterFor st stone]

    -- With nothing to drink and nothing to spend, it walks. Three of the
    -- four ways out of that tile are equally far from the wolf, so what
    -- matters is the one it does not take: the one into the wolf.
    it "walks away when it has nothing left to spend" $ do
      let st = wolves 1 (carrying [] (atHealth 3))
      presses st `shouldSatisfy` maybe False (`elem` [Just "w", Just "a", Just "s"]) . Just
      presses st `shouldNotBe` Just ['d']

    it "stands and fights while it is healthy" $ do
      let st = wolves 1 (carrying [] (atHealth 100))
      presses st `shouldBe` Just ['d']

  hasWorld <- runIO (doesFileExist "world.json")
  if not hasWorld
    then it "requires world.json" $ pendingWith "run the test-suite from the package root"
    else describe "the dungeon in world.json" $ do
      let played seed = do
            config <- loadNewGame defaultWorldFile
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
