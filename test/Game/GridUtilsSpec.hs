-- test/Game/GridUtilsSpec.hs
module Game.GridUtilsSpec (spec) where

import Game.GridUtils (keyedInventory, updateTile)
import Game.Types
import Linear.V2 (V2 (..))
import Test.Hspec
import Test.QuickCheck

import Fixtures

-- A 4x3 grid of floor tiles.
floorGrid :: [[Tile]]
floorGrid = replicate 3 (replicate 4 Floor)

spec :: Spec
spec = do
  describe "updateTile" $ do
    it "sets the tile at the given (x, y)" $
      updateTile floorGrid (2, 1) Wall !! 1 !! 2 `shouldBe` Wall

    it "leaves every other tile untouched" $ do
      let grid' = updateTile floorGrid (2, 1) Wall
          others = [(x, y) | y <- [0 .. 2], x <- [0 .. 3], (x, y) /= (2, 1)]
      map (\(x, y) -> grid' !! y !! x) others `shouldSatisfy` all (== Floor)

    it "preserves the dimensions of the grid" $
      property $ \(NonNegative x) (NonNegative y) ->
        let grid' = updateTile floorGrid (x `mod` 4, y `mod` 3) Wall
         in map length grid' === map length floorGrid

  describe "keyedInventory" $ do
    let sword  = mkItem "Sword" Weapon 4 (V2 0 0)
        shield = mkItem "Shield" Armor 2 (V2 1 0)
        potion = mkItem "Potion" Healing 5 (V2 2 0)

    it "keys the inventory from 'a' upwards" $
      map fst (keyedInventory [sword, shield, potion] Nothing Nothing)
        `shouldBe` "abc"

    it "keeps the inventory order when nothing is equipped" $
      map snd (keyedInventory [sword, shield, potion] Nothing Nothing)
        `shouldBe` [sword, shield, potion]

    it "lists the equipped weapon and armor first" $
      map snd (keyedInventory [potion, sword, shield] (Just sword) (Just shield))
        `shouldBe` [sword, shield, potion]

    it "returns exactly one entry per item" $
      length (keyedInventory [potion, sword, shield] (Just sword) Nothing)
        `shouldBe` 3

    it "is empty for an empty inventory" $
      keyedInventory [] Nothing Nothing `shouldBe` []
