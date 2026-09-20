-- test/UI/ScreenSpec.hs
--
-- What the player actually sees. UI.DrawSpec checks the lookup tables the
-- renderer consults; this checks the glyphs and panels that come out the
-- other end, which is the part a player would notice being wrong.
module UI.ScreenSpec (spec) where

import Data.List (isInfixOf)
import Game.GridUtils (keyedInventory)
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
      map (glyphAt aiming . mPosition . snd) (getVisibleMonsters aiming)
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
    it "shows the help only once it is opened" $ do
      showsText room "Move up" `shouldBe` False
      showsText room {legendPage = 1} "Move up" `shouldBe` True

    it "shows the victory screen only after winning" $ do
      showsText room "You got out alive" `shouldBe` False
      showsText room {gameWon = True} "You got out alive" `shouldBe` True

  describe "the end of a run" $ do
    it "shows nothing special while the player is alive" $ do
      showsText room "You have died" `shouldBe` False
      showsText room "Game Over" `shouldBe` False

    it "shows a banner once the player has died" $
      showsText room {gameOver = True} "You have died" `shouldBe` True

    it "says how to get out of it" $ do
      let dead = room {gameOver = True}
      showsText dead ":restart" `shouldBe` True
      showsText dead ":q" `shouldBe` True

    it "shows the victory screen rather than the death one after winning" $ do
      showsText room {gameWon = True} "You got out alive" `shouldBe` True
      showsText room {gameWon = True} "You have died" `shouldBe` False

  describe "the run summary" $ do
    let hauling =
          withPlayer (\p -> p {inventory = [mkTreasure "Gold Coin" 250, mkTreasure "Crown" 900], xp = 77})
            room {deepestLevel = 2}

    it "counts what is being carried, on screen, while there is still a choice" $
      showsText hauling "Treasure: 1150" `shouldBe` True

    it "reports depth, treasure and experience on getting out" $ do
      let out = hauling {gameWon = True}
      showsText out "Reached level 3" `shouldBe` True
      showsText out "Treasure carried out: 1150" `shouldBe` True
      showsText out "Experience: 77" `shouldBe` True

    it "calls the same treasure lost when the run ends badly" $ do
      let dead = hauling {gameOver = True}
      showsText dead "Treasure lost: 1150" `shouldBe` True
      showsText dead "Treasure carried out" `shouldBe` False

    it "counts nothing when the pack is empty" $
      showsText room "Treasure: 0" `shouldBe` True

  describe "choosing an item" $ do
    let stocked =
          withPlayer (\p -> p {inventory = [mkItem ("Item" ++ show i) Special 0 (V2 0 0) | i <- [1 .. 15 :: Int]]}) room
        choosing m = stocked {inventoryMode = Just m}
        keysOn sz st =
          [ k
          | k <- take 15 ['a' ..]
          , any ((k : ") Item") `isInfixOf`) (renderRows sz (drawUI st))
          ]

    -- The sidebar cannot show fifteen items on a short terminal: it needs 27
    -- rows and there are only 13 to give it. The keys have to be reachable
    -- anyway, because they are what the player is about to press.
    it "shows every key on a small terminal while choosing" $
      keysOn (80, 24) (choosing UseMode) `shouldBe` take 15 ['a' ..]

    it "shows every key on a large terminal while choosing" $
      keysOn (120, 40) (choosing UseMode) `shouldBe` take 15 ['a' ..]

    it "says whether the item is being used or dropped" $ do
      showsText (choosing UseMode) "Use which item?" `shouldBe` True
      showsText (choosing DropMode) "Drop which item?" `shouldBe` True

    it "shows no chooser when the player is not choosing" $
      showsText stocked "which item?" `shouldBe` False

    it "keys the chooser the same way the selection logic does" $
      map fst (keyedInventory (inventory (player stocked)) Nothing Nothing)
        `shouldBe` keysOn (120, 40) (choosing UseMode)

  describe "what the player is standing on" $ do
    it "names an item underfoot" $ do
      let st = withWorld (\w -> w {items = [mkItem "Ruby Amulet" Special 0 (V2 4 3)]}) room
      showsText st "You see: Ruby Amulet" `shouldBe` True

    it "says nothing when the tile is empty" $
      showsText room "You see:" `shouldBe` False

    it "does not announce an item that has not spawned yet" $ do
      let st = withWorld (\w -> w {items = [(mkItem "Later" Special 0 (V2 4 3)) {iInactive = True}]}) room
      showsText st "You see:" `shouldBe` False
