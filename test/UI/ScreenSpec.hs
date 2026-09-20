-- test/UI/ScreenSpec.hs
--
-- What the player actually sees. UI.DrawSpec checks the lookup tables the
-- renderer consults; this checks the glyphs and panels that come out the
-- other end, which is the part a player would notice being wrong.
module UI.ScreenSpec (spec) where

import Data.List (isInfixOf)
import Game.Logic (getVisibleMonsters)
import Game.State (withCurrentWorld)
import Game.Types
import Linear.V2 (V2 (..))
import Test.Hspec
import UI.Draw (drawUI)

import Fixtures
import Render

size :: (Int, Int)
size = (120, 40)

screen :: GameState -> [String]
screen st = renderRows size (drawUI st)

showsText :: GameState -> String -> Bool
showsText st needle = any (needle `isInfixOf`) (screen st)

-- Light the whole room so nothing is hidden by fog.
lit :: GameState -> GameState
lit = withCurrentWorld (\w -> w {visibility = replicate 7 (replicate 9 True)})

-- The fixture room, fully lit, with the player at (4, 3).
room :: GameState
room = lit baseState

-- The glyph drawn at a map position. The map border starts at column 0, so
-- the tile at (x, y) is one row and one column in from it.
glyphAt :: GameState -> V2 Int -> Char
glyphAt st (V2 x y) =
  let rows = screen st
      top = length (takeWhile ((/= "\9484") . take 1) rows)
      row = rows !! (top + 1 + y)
   in row !! (1 + x)

spec :: Spec
spec = do
  describe "the map" $ do
    it "draws the player" $
      glyphAt room (V2 4 3) `shouldBe` '@'

    it "draws floor and wall" $ do
      glyphAt room (V2 1 1) `shouldBe` '.'
      glyphAt room (V2 0 0) `shouldBe` '#'

    it "draws a monster, an item, an NPC and a corpse" $ do
      let st = withWorld
                 (\w -> w { monsters = [mkMonster "Goblin" (V2 1 1) 5 1]
                          , items = [mkItem "Sword" Weapon 4 (V2 2 1)]
                          , npcs = [mkNPC "Bob" (V2 3 1)]
                          , corpses = [V2 5 1]
                          })
                 room
      map (glyphAt st) [V2 1 1, V2 2 1, V2 3 1, V2 5 1] `shouldBe` "M!N\8224"

    it "draws the player over anything sharing the tile" $ do
      let st = withWorld (\w -> w {items = [mkItem "Sword" Weapon 4 (V2 4 3)], corpses = [V2 4 3]}) room
      glyphAt st (V2 4 3) `shouldBe` '@'

    it "draws a monster over an item" $ do
      let st = withWorld
                 (\w -> w { monsters = [mkMonster "Goblin" (V2 1 1) 5 1]
                          , items = [mkItem "Sword" Weapon 4 (V2 1 1)]
                          })
                 room
      glyphAt st (V2 1 1) `shouldBe` 'M'

    it "hides an item that is hidden or not spawned yet" $ do
      let st = withWorld
                 (\w -> w {items = [ (mkItem "Secret" Special 0 (V2 1 1)) {iHidden = True}
                                   , (mkItem "Later" Special 0 (V2 2 1)) {iInactive = True}
                                   ]})
                 room
      map (glyphAt st) [V2 1 1, V2 2 1] `shouldBe` ".."

  describe "fog of war" $ do
    -- A fresh world has been neither seen nor visited.
    let dark = baseState
        seenBefore =
          withCurrentWorld (\w -> w {discovered = replicate 7 (replicate 9 True)}) baseState

    it "leaves an unexplored tile blank" $
      glyphAt dark (V2 1 1) `shouldBe` ' '

    it "shows remembered terrain, with walls still walls" $ do
      glyphAt seenBefore (V2 0 0) `shouldBe` '#'
      glyphAt seenBefore (V2 1 1) `shouldBe` '.'

    it "does not show a monster standing in a remembered but unlit room" $ do
      let st = withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 1 1) 5 1]}) seenBefore
      glyphAt st (V2 1 1) `shouldBe` '.'

  describe "aiming a ranged attack" $ do
    let bow = mkItem "Bow" Range 6 (V2 0 0)
        withMonsters =
          withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 1 1) 5 1, mkMonster "Rat" (V2 2 2) 3 1]}) room
        aiming = withMonsters {aimingState = Just (AimingState bow)}

    it "labels the monsters instead of drawing them as M" $
      map (glyphAt aiming) [V2 1 1, V2 2 2] `shouldBe` "ab"

    it "labels them the same way the attack logic does" $
      map (glyphAt aiming) (map (mPosition . snd) (getVisibleMonsters aiming))
        `shouldBe` map fst (getVisibleMonsters aiming)

    it "goes back to M when not aiming" $
      map (glyphAt withMonsters) [V2 1 1, V2 2 2] `shouldBe` "MM"

  describe "the side panels" $ do
    it "shows the player's stats" $ do
      let st = withPlayer (\p -> p {health = 17, xp = 42, playerXPLevel = 3}) room
      mapM_ (\t -> showsText st t `shouldBe` True) ["Level: 3", "HP: 17", "XP: 42", "Attack:", "Resistance:"]

    it "says when the inventory is empty" $
      showsText room "No items collected" `shouldBe` True

    it "lists items against the keys used to select them" $ do
      let st = withPlayer (\p -> p {inventory = [mkItem "Sword" Weapon 4 (V2 0 0), mkItem "Shield" Armor 2 (V2 0 0)]}) room
      showsText st "a) Sword" `shouldBe` True
      showsText st "b) Shield" `shouldBe` True

    it "marks the equipped weapon and armor" $ do
      let sword = mkItem "Sword" Weapon 4 (V2 0 0)
          shield = mkItem "Shield" Armor 2 (V2 0 0)
          st = withPlayer (\p -> p {inventory = [sword, shield], equippedWeapon = Just sword, equippedArmor = Just shield}) room
      showsText st "Sword (W)" `shouldBe` True
      showsText st "Shield (A)" `shouldBe` True

    it "shows how many uses are left" $ do
      let potion = (mkItem "Potion" Healing 5 (V2 0 0)) {iUses = Just 3}
          st = withPlayer (\p -> p {inventory = [potion]}) room
      showsText st "Potion (3)" `shouldBe` True

    it "shows every key of a full inventory" $ do
      let st = withPlayer (\p -> p {inventory = [mkItem ("Item" ++ show i) Special 0 (V2 0 0) | i <- [1 .. maxInventory]]}) room
          maxInventory = 15
      mapM_ (\k -> showsText st (k : ") Item") `shouldBe` True) (take maxInventory ['a' ..])

  describe "the command line" $ do
    it "echoes what has been typed" $
      showsText room {commandBuffer = ":resta"} ":resta" `shouldBe` True

    it "shows the prompt with an empty buffer" $
      showsText room "Command:" `shouldBe` True

  describe "popups" $ do
    it "shows the legend only when it is toggled on" $ do
      showsText room "Toggle this help" `shouldBe` False
      showsText room {showLegend = True} "Toggle this help" `shouldBe` True

    it "shows the victory screen only after winning" $ do
      showsText room "You have won the game!" `shouldBe` False
      showsText room {gameWon = True} "You have won the game!" `shouldBe` True

  describe "what the player is standing on" $ do
    it "names an item underfoot" $ do
      let st = withWorld (\w -> w {items = [mkItem "Ruby Amulet" Special 0 (V2 4 3)]}) room
      showsText st "You see: Ruby Amulet" `shouldBe` True

    it "says nothing when the tile is empty" $
      showsText room "You see:" `shouldBe` False

    it "does not announce an item that has not spawned yet" $ do
      let st = withWorld (\w -> w {items = [(mkItem "Later" Special 0 (V2 4 3)) {iInactive = True}]}) room
      showsText st "You see:" `shouldBe` False
