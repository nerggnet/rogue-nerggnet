-- test/UI/ScreenSpec.hs
--
-- What the player actually sees. UI.DrawSpec checks the lookup tables the
-- renderer consults; this checks the glyphs and panels that come out the
-- other end, which is the part a player would notice being wrong.
module UI.ScreenSpec (spec) where

import Data.List (dropWhileEnd, isInfixOf, isPrefixOf, isSuffixOf)
import Game.GridUtils (keyedInventory)
import Game.Logic (getVisibleMonsters)
import Game.State (rousingInterval, withCurrentWorld)
import Game.Types
import Linear.V2 (V2 (..))
import Test.Hspec
import Brick (attrMapLookup, attrName)
import Graphics.Vty.Attributes
import UI.Draw (drawUI)
import UI.MainUI (defaultAttrMap)

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

  describe "monsters that shoot" $ do
    let shooter = (mkMonster "Bone Archer" (V2 6 3) 30 9) {mRange = Just 4}
        melee = mkMonster "Cave Rat" (V2 2 3) 30 9

    -- A letter of its own, not a shade of the same one: whether the thing
    -- across the room can hit you from there decides whether you cross it.
    it "draws one that shoots apart from one that does not" $ do
      let st = withWorld (\w -> w {monsters = [shooter, melee]}) room
      glyphAt st (V2 6 3) `shouldBe` 'A'
      glyphAt st (V2 2 3) `shouldBe` 'M'

    it "still draws it as a monster once it is inactive" $ do
      let st = withWorld (\w -> w {monsters = [shooter {mInactive = True}]}) room
      glyphAt st (V2 6 3) `shouldNotBe` 'A'

  describe "how the map is coloured" $ do
    let attrOf n = attrMapLookup (attrName n) defaultAttrMap

    -- The shade is what tells remembered ground from lit, and it is the one
    -- 24-bit colour in the game: a terminal with eight colours drops it and
    -- is left with no cue at all. The dim style is one such a terminal can
    -- show, and vty leaves it off where even that is unsupported.
    it "marks remembered ground with a style as well as a shade" $ do
      attrStyle (attrOf "discovered") `shouldBe` SetTo dim
      attrBackColor (attrOf "discovered") `shouldNotBe` Default

    -- Nothing is told by colour alone; every entity has a glyph of its own.
    it "asks for nothing but the eight plain colours elsewhere" $ do
      let plain = [black, white, yellow, green, red, blue, magenta, cyan]
          named = ["door", "upStair", "downStair", "shaft", "player",
                   "monster", "shooter", "aimingMonster", "corpse", "npc", "item"]
      [n | n <- named, attrForeColor (attrOf n) `notElem` map SetTo plain]
        `shouldBe` []

  describe "doorways" $ do
    let withDoor d = withWorld (\w -> w {doors = [d]})
                       (withCurrentWorld (putDoor (dePosition d)) room)
        putDoor (V2 dx dy) w =
          w {mapGrid = [[if (x, y) == (dx, dy) then Door else t
                         | (x, t) <- zip [0 :: Int ..] row]
                        | (y, row) <- zip [0 :: Int ..] (mapGrid w)]}

    -- Whether a doorway can be walked through decides whether a corridor
    -- is a way out or a wall, and it changes as the player works the door.
    it "draws one standing open apart from one that is shut" $ do
      glyphAt (withDoor (mkDoor (V2 6 3) False "Iron Key")) (V2 6 3) `shouldBe` '\''
      glyphAt (withDoor ((mkDoor (V2 6 3) False "Iron Key") {deShut = True})) (V2 6 3) `shouldBe` '+'

    it "draws a locked one shut, because it is" $
      glyphAt (withDoor (mkDoor (V2 6 3) True "Iron Key")) (V2 6 3) `shouldBe` '+'

    -- An open door was drawn as two characters for a while, which shifted
    -- every tile to the right of it one place and pushed the row wider than
    -- the map, so the whole thing scrolled inside its own border. Checking
    -- the glyph itself did not catch it, because the first of the two was
    -- the right one: what has to be true is that the tiles after it are
    -- still where they belong.
    it "draws each tile in exactly one column, doors included" $ do
      let openDoor = withDoor (mkDoor (V2 2 3) False "Iron Key")
          marked = withCurrentWorld
            (\w -> w {mapGrid = [[if (x, y) == (7, 3) then DownStair else t
                                  | (x, t) <- zip [0 :: Int ..] row]
                                 | (y, row) <- zip [0 :: Int ..] (mapGrid w)]})
            openDoor
      glyphAt marked (V2 2 3) `shouldBe` '\''
      glyphAt marked (V2 7 3) `shouldBe` '>'


  describe "the grave of an earlier run" $ do
    let buried extra = withWorld (\w -> w {graves = [Grave "yesterday" "d" 0 (V2 6 3) [] 90]
                                          , items = extra}) room

    -- Told apart from a monster's corpse on purpose: one is something the
    -- player killed, the other is the player.
    it "is marked, and not with the mark a kill leaves" $ do
      glyphAt (buried []) (V2 6 3) `shouldBe` '\8225'
      glyphAt (buried []) (V2 6 3) `shouldNotBe` '\8224'

    -- What it was carrying is lying on it, and that is what you came for.
    it "shows its belongings while they are still there" $
      glyphAt (buried [mkItem "Gold Coin" Special 0 (V2 6 3)]) (V2 6 3) `shouldBe` '!'

  describe "a trap that has gone off" $ do
    let atTrap = withWorld (\w -> w {sprung = [V2 6 3]}) room
        standingIn = withPlayer (\p -> p {position = V2 6 3}) atTrap

    -- A line in the log scrolls away; a mark on the floor is still there
    -- when the player looks up.
    it "leaves a mark on the floor" $
      glyphAt atTrap (V2 6 3) `shouldBe` '*'

    -- Standing in one, the player covers it, so the player has to show it.
    it "shows on the player while they are standing in it" $
      glyphAt standingIn (V2 6 3) `shouldBe` '@'

    it "is not the shaft mark; a way up is not a blade" $
      glyphAt atTrap (V2 6 3) `shouldNotBe` '^'

    it "does not lie on top of a way out, any more than a corpse does" $ do
      let onStairs = withCurrentWorld
            (\w -> w {mapGrid = [[if (x, y) == (6, 3) then DownStair else t
                                  | (x, t) <- zip [0 :: Int ..] row]
                                 | (y, row) <- zip [0 :: Int ..] (mapGrid w)]})
            atTrap
      glyphAt onStairs (V2 6 3) `shouldBe` '>'

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

    -- A staircase you have walked past is a staircase you remember. Drawing
    -- it as plain floor meant a floor mapped by a Miner's Lantern showed
    -- everything except the one thing a map is for.
    it "remembers a way out it has seen" $ do
      let withStairs = withCurrentWorld
            (\w -> w {mapGrid = [[if (x, y) == (3, 1) then DownStair
                                   else if (x, y) == (5, 1) then Door
                                   else t | (x, t) <- zip [0 :: Int ..] row]
                                 | (y, row) <- zip [0 :: Int ..] (mapGrid w)]})
            seenBefore
      glyphAt withStairs (V2 3 1) `shouldBe` '>'
      glyphAt withStairs (V2 5 1) `shouldBe` '+'

  describe "corpses" $ do
    let died pos = withWorld (\w -> w {corpses = [pos]}) room
        onStairs = withCurrentWorld
          (\w -> w {mapGrid = [[if (x, y) == (6, 3) then DownStair else t
                                | (x, t) <- zip [0 :: Int ..] row]
                               | (y, row) <- zip [0 :: Int ..] (mapGrid w)]})
          (died (V2 6 3))

    it "marks where something died" $
      glyphAt (died (V2 6 3)) (V2 6 3) `shouldBe` '\8224'

    -- Something died on the stairs down on floor 7 and the marker sat on
    -- top of them for the rest of the run: a player could stand on the way
    -- down and be told nothing.
    it "does not lie on top of a way out" $
      glyphAt onStairs (V2 6 3) `shouldBe` '>'

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

  describe "the title bar" $ do
    let threeDeep n =
          let w = mkWorld openMap
           in (room {levels = replicate 3 w, currentLevel = n})

    it "says which floor the player is on, and how deep it goes" $
      showsText (threeDeep 1) "Floor 2 of 3" `shouldBe` True

    it "counts floors from one, the way the player does" $
      showsText (threeDeep 0) "Floor 1 of 3" `shouldBe` True

    it "keeps saying how to reach the help" $
      showsText room "press ? for help" `shouldBe` True

  describe "the side panels" $ do
    it "shows the player's stats" $ do
      let st = withPlayer (\p -> p {health = 17, xp = 42, playerXPLevel = 3}) room
      mapM_ (\t -> showsText st t `shouldBe` True) ["XP level: 3", "HP: 17", "XP: 42", "Attack:", "Resistance:"]

    -- The table in the fixture goes 0, 100, 250 and stops.
    it "says how much more experience the next level wants" $ do
      let st = withPlayer (\p -> p {xp = 42, playerXPLevel = 1}) room
      showsText st "XP: 42 (58 to next)" `shouldBe` True

    it "counts from the rung above, not from the one just passed" $ do
      let st = withPlayer (\p -> p {xp = 120, playerXPLevel = 2}) room
      showsText st "XP: 120 (130 to next)" `shouldBe` True

    it "says so at the top of the table, where there is no next" $ do
      let st = withPlayer (\p -> p {xp = 900, playerXPLevel = 3}) room
      showsText st "XP: 900 (top XP level)" `shouldBe` True

    -- The stats box is thirty columns including its border, and the numbers
    -- deepest in the dungeon are five digits each. Asserting the line is
    -- short enough proves nothing, because Brick clips it to the box either
    -- way; what has to be true is that the whole of it survives.
    it "shows the whole line with the dungeon's largest numbers" $ do
      let deep = [XPLevel {xpLevel = 19, xpThreshold = 75000, xpHealth = 2140,
                           xpAttack = 50, xpResistance = 30}]
          st = (withPlayer (\p -> p {xp = 68533, playerXPLevel = 18}) room)
                 {xpLevels = deep}
      showsText st "XP: 68533 (6467 to next)" `shouldBe` True

    -- "(top XP level)" is three characters longer than the "(top level)" it
    -- replaced, and a player standing on the top rung of the shipped table
    -- has six digits of experience to put in front of it.
    it "shows the whole of the top-of-table line at those numbers too" $ do
      let top = [XPLevel {xpLevel = 20, xpThreshold = 92000, xpHealth = 2350,
                          xpAttack = 53, xpResistance = 32}]
          st = (withPlayer (\p -> p {xp = 123456, playerXPLevel = 20}) room)
                 {xpLevels = top}
      showsText st "XP: 123456 (top XP level)" `shouldBe` True

    -- The dungeon rouses on a clock, so the clock has to be on screen:
    -- a cost the player cannot see is not one they can decide about.
    it "shows the turn the run is on" $
      showsText room {turnCount = 1234} "Turn: 1234" `shouldBe` True

    it "says nothing about rousing while the dungeon is calm" $
      showsText room {turnCount = 1} "roused" `shouldBe` False

    it "says how roused the dungeon is once it has stirred" $
      showsText room {turnCount = 3 * rousingInterval} "(roused +6%)" `shouldBe` True

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

    it "keeps the leading colon the player typed, and adds none of its own" $ do
      let rows = renderRows (terminal 100 40) (drawUI room {commandBuffer = ":restart"})
      map (dropWhileEnd (== ' ')) (filter (":" `isPrefixOf`) rows) `shouldBe` [":restart"]

    it "says nothing at all while the line is closed" $
      showsText room "Command" `shouldBe` False

  describe "the message history" $ do
    let said n = ["line " ++ show i | i <- [n, n - 1 .. 1 :: Int]]  -- newest first
        looking n = room {message = said n, showLog = True}

    it "shows nothing until it is asked for" $
      showsText room {message = said 3} "Messages (" `shouldBe` False

    -- The pane on the main screen shows five; the point of this is the ones
    -- that have gone past it.
    it "shows messages the pane has scrolled past" $ do
      showsText (looking 12) "line 1" `shouldBe` True
      showsText (looking 12) "line 12" `shouldBe` True

    it "reads oldest first, the way the pane does" $ do
      let rows = screen (looking 6)
          rowOf needle = length (takeWhile (not . (needle `isInfixOf`)) rows)
      rowOf "line 6" `shouldSatisfy` (> rowOf "line 1")

    -- Naming the range rather than the count is what makes scrolling
    -- legible: "16 of 40" looks the same on every line of the history.
    it "says which lines of the history it is showing" $
      showsText (looking 40) "Messages (25-40 of 40)" `shouldBe` True

    it "says so again after scrolling back" $ do
      let back = (looking 40) {logScroll = 10}
      showsText back "Messages (15-30 of 40)" `shouldBe` True
      showsText back "line 15" `shouldBe` True
      showsText back "line 30" `shouldBe` True
      showsText back "line 31" `shouldBe` False

    it "offers the scrolling keys when there is more than one screenful" $
      showsText (looking 40) "j/k to scroll" `shouldBe` True

    -- Nothing to scroll, so nothing to say about scrolling.
    it "offers them nowhere else" $
      showsText (looking 3) "j/k to scroll" `shouldBe` False

    it "says so when nothing has happened yet" $
      showsText room {message = [], showLog = True} "Nothing has happened yet" `shouldBe` True

    -- Sixteen lines plus a border and a hint is what fits the shortest
    -- terminal the game supports. Both ends of the window and the hint
    -- under it have to survive, or the popup is being clipped by the
    -- terminal rather than sized to it.
    it "fits an 80x24 terminal" $ do
      let rows = renderRows (80, 24) (drawUI (looking 60))
          titleRow = filter ("Messages (" `isInfixOf`) rows
      map (dropWhileEnd (== ' ')) titleRow `shouldSatisfy` all (("\9488" :: String) `isSuffixOf`)
      rows `shouldSatisfy` any ("line 45" `isInfixOf`)
      rows `shouldSatisfy` any ("line 60" `isInfixOf`)
      rows `shouldSatisfy` any ("any other key closes" `isInfixOf`)

  describe "the scoreboard" $ do
    let aRun n = Run { runWhen = "2026-01-0" ++ show n ++ " 12:00", runEnding = GotOut
                     , runDepth = n, runTreasure = 100 * n, runXP = 10, runTurns = 50 }
        board = room {scoreboard = [aRun 1, aRun 2], showScores = True}

    it "shows nothing until it is asked for" $
      showsText room "score" `shouldBe` False

    it "lists the runs recorded so far" $ do
      showsText board "2026-01-01 12:00" `shouldBe` True
      showsText board "2026-01-02 12:00" `shouldBe` True

    -- aRun 2 went deeper and carried more out, so it belongs above aRun 1.
    it "puts the better run first" $ do
      let rowOf needle = length (takeWhile (not . (needle `isInfixOf`)) (screen board))
      rowOf "2026-01-02" `shouldSatisfy` (< rowOf "2026-01-01")

    it "says so when there is nothing recorded yet" $
      showsText room {showScores = True} "Nothing recorded yet" `shouldBe` True

    -- The run just finished is not on the board until the game exits, so
    -- the end screen has to put it there itself.
    it "marks the run just finished on the end screen" $ do
      let ended = room {gameWon = True, deepestLevel = 3, scoreboard = [aRun 1]}
      showsText ended "this run" `shouldBe` True

    it "ranks the finished run against the recorded ones" $ do
      let ended = room {gameWon = True, deepestLevel = 11, scoreboard = [aRun 1]}
          rows = filter (\r -> "this run" `isInfixOf` r) (screen ended)
      rows `shouldSatisfy` any (">" `isInfixOf`)

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
      showsText out "Reached floor 3" `shouldBe` True
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

    -- The second column is what the chooser is for now, so it has to be
    -- there; an item list of bare names says nothing a player can act on.
    it "says what each item does" $ do
      let potion = (mkItem "Health Potion" Healing 60 (V2 0 0)) {iUses = Just 2}
          st = (withPlayer (\p -> p {inventory = [potion]}) room) {inventoryMode = Just UseMode}
      showsText st "heals 60" `shouldBe` True

    it "explains a Special by its effect, not its name" $ do
      let scroll = mkSpecial "Ashen Scroll" Firestorm 60
          st = (withPlayer (\p -> p {inventory = [scroll]}) room) {inventoryMode = Just UseMode}
      showsText st "60 damage to all in sight" `shouldBe` True

    -- The whole popup has to fit the narrowest terminal the game supports,
    -- or the column it was widened for is the first thing off the screen.
    it "keeps the chooser inside an 80-column terminal" $ do
      let wordy = [ (mkItem ("Reliquary of the Deepest Deep " ++ show i) Special 0 (V2 0 0))
                      {iEffect = Just Lifesteal, iEffectValue = 25}
                  | i <- [1 .. 15 :: Int] ]
          st = (withPlayer (\p -> p {inventory = wordy}) room) {inventoryMode = Just UseMode}
          rows = renderRows (80, 24) (drawUI st)
      -- What matters is that the second column is still readable: Brick
      -- clips the popup to the terminal either way, so a test that only
      -- looked for the border passed whatever the column widths were.
      rows `shouldSatisfy` any ("returns 25% of damage dealt, while carried" `isInfixOf`)
      -- ...and that the names were what gave way, marked as cut.
      rows `shouldSatisfy` any ("~" `isInfixOf`)

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
