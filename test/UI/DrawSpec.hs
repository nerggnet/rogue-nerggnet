-- test/UI/DrawSpec.hs
--
-- The widget assembly is not worth asserting on, but the lookup tables that
-- decide what each tile shows are, and they are what the per-frame hoist
-- moved out of the tile renderer.
module UI.DrawSpec (spec) where

import Game.Types
import Linear.V2 (V2 (..))
import Test.Hspec
import UI.Draw (MapView (..), mapView)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Fixtures

-- A fully lit room, so visibility never hides anything by accident.
litRoom :: World
litRoom = (mkWorld openMap) {visibility = replicate 7 (replicate 9 True)}

viewOf :: World -> MapView
viewOf world = mapView world (mkPlayer (V2 4 3)) Nothing

spec :: Spec
spec = describe "mapView" $ do
  it "records where the player is" $
    viewPlayer (viewOf litRoom) `shouldBe` V2 4 3

  it "is not aiming unless a ranged item is in hand" $ do
    viewAiming (viewOf litRoom) `shouldBe` False
    viewAiming
      ( mapView litRoom (mkPlayer (V2 4 3))
          (Just (AimingState (mkItem "Bow" Range 5 (V2 0 0))))
      )
      `shouldBe` True

  it "collects active monster positions" $
    viewMonsters
      ( viewOf
          litRoom {monsters = [mkMonster "Goblin" (V2 1 1) 5 1, mkMonster "Rat" (V2 2 2) 3 1]}
      )
      `shouldBe` Set.fromList [V2 1 1, V2 2 2]

  it "leaves inactive spawn templates off the map" $
    viewMonsters
      (viewOf litRoom {monsters = [(mkMonster "Dragon" (V2 1 1) 50 9) {mInactive = True}]})
      `shouldBe` Set.empty

  it "letters the visible monsters for targeting" $
    viewLetters
      ( viewOf
          litRoom {monsters = [mkMonster "Goblin" (V2 1 1) 5 1, mkMonster "Rat" (V2 2 2) 3 1]}
      )
      `shouldBe` Map.fromList [(V2 1 1, 'a'), (V2 2 2, 'b')]

  it "letters nothing that is standing in the dark" $
    viewLetters (viewOf (mkWorld openMap) {monsters = [mkMonster "Goblin" (V2 1 1) 5 1]})
      `shouldBe` Map.empty

  it "shows items that are on the floor" $
    viewItems (viewOf litRoom {items = [mkItem "Sword" Weapon 4 (V2 3 3)]})
      `shouldBe` Set.fromList [V2 3 3]

  it "hides items that are hidden or not spawned yet" $
    viewItems
      ( viewOf
          litRoom
            { items =
                [ (mkItem "Secret" Special 0 (V2 3 3)) {iHidden = True}
                , (mkItem "Later" Special 0 (V2 4 4)) {iInactive = True}
                ]
            }
      )
      `shouldBe` Set.empty

  it "collects NPC positions" $
    viewNpcs (viewOf litRoom {npcs = [mkNPC "Bob" (V2 2 2)]})
      `shouldBe` Set.fromList [V2 2 2]

  it "collects corpse positions" $
    viewCorpses (viewOf litRoom {corpses = [V2 5 3, V2 6 3]})
      `shouldBe` Set.fromList [V2 5 3, V2 6 3]

  it "is empty for an untouched level" $ do
    let v = viewOf litRoom
    viewMonsters v `shouldBe` Set.empty
    viewItems v `shouldBe` Set.empty
    viewNpcs v `shouldBe` Set.empty
    viewCorpses v `shouldBe` Set.empty
    viewLetters v `shouldBe` Map.empty
