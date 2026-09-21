-- test/Game/LogicSpec.hs
module Game.LogicSpec (spec) where

import Data.List (isInfixOf, nub, unfoldr, (\\))
import Game.Logic
import Game.State (currentWorld, evalTriggerCondition, helpPages, maxInventorySize, maxLogMessages, visibleMonsters)
import Game.Types
import System.Random (mkStdGen)
import Linear.V2 (V2 (..))
import Test.Hspec
import Test.QuickCheck

import Fixtures

-- Damage is rolled, so a test asserts the band rather than one figure.
onlyBetween :: Int -> Int -> [Int] -> Bool
onlyBetween lo hi [x] = lo <= x && x <= hi
onlyBetween _ _ _ = False

-- The most recent line in the message log.
latest :: GameState -> String
latest state = case message state of
  []      -> ""
  (m : _) -> m

spec :: Spec
spec = do
  describe "isAdjacent" $ do
    it "holds for orthogonal neighbours" $
      map (isAdjacent (V2 4 4)) [V2 3 4, V2 5 4, V2 4 3, V2 4 5]
        `shouldBe` [True, True, True, True]

    it "does not hold diagonally" $
      isAdjacent (V2 0 0) (V2 1 1) `shouldBe` False

    it "does not hold for a tile against itself" $
      isAdjacent (V2 0 0) (V2 0 0) `shouldBe` False

  describe "initSafe" $ do
    it "returns the empty list unchanged" $
      initSafe ([] :: [Int]) `shouldBe` []

    it "drops the last element" $
      initSafe [1, 2, 3 :: Int] `shouldBe` [1, 2]

  describe "replaceFirst" $ do
    it "swaps only the first matching element" $
      replaceFirst 1 9 [1, 2, 1 :: Int] `shouldBe` [9, 2, 1]

    it "leaves a list without a match alone" $
      replaceFirst 7 9 [1, 2, 3 :: Int] `shouldBe` [1, 2, 3]

  describe "movePlayer" $ do
    it "steps onto an adjacent floor tile" $
      position (player (movePlayer East baseState)) `shouldBe` V2 5 3

    it "moves in all four directions" $
      map (\d -> position (player (movePlayer d baseState)))
        [North, South, East, West]
        `shouldBe` [V2 4 2, V2 4 4, V2 5 3, V2 3 3]

    it "refuses to walk into a wall" $ do
      let atTop = mkState (mkWorld openMap) (V2 4 1)
      position (player (movePlayer North atTop)) `shouldBe` V2 4 1

    it "is blocked by a locked door" $ do
      let s = movePlayer East (withWorld (\w -> w {doors = [mkDoor (V2 5 3) True "Iron Key"]}) baseState)
      position (player s) `shouldBe` V2 4 3
      latest s `shouldSatisfy` ("locked" `isInfixOf`)

    it "walks through an unlocked door" $ do
      let s = movePlayer East (withWorld (\w -> w {doors = [mkDoor (V2 5 3) False "Iron Key"]}) baseState)
      position (player s) `shouldBe` V2 5 3

    -- Talking used to leave both of you where you were, which made an NPC a
    -- wall. NPCs step aside only on their own clock, and one with the player
    -- on one side and a monster on the other has nowhere to step: in a
    -- one-tile corridor that sealed the passage for good.
    it "talks to an NPC and changes places with it" $ do
      let s = movePlayer East (withWorld (\w -> w {npcs = [mkNPC "Bob" (V2 5 3)]}) baseState)
      position (player s) `shouldBe` V2 5 3
      map npcPosition (npcs (currentWorld s)) `shouldBe` [V2 4 3]
      lastInteractedNpc s `shouldBe` Just "Bob"
      latest s `shouldSatisfy` ("Bob says: hello" `isInfixOf`)

    it "still talks, without moving, when the NPC stands in a wall" $ do
      let boxed = withWorld (\w -> w {npcs = [mkNPC "Bob" (V2 5 0)]})
                    (mkState (mkWorld openMap) (V2 5 1))
          s = movePlayer North boxed
      position (player s) `shouldBe` V2 5 1
      lastInteractedNpc s `shouldBe` Just "Bob"

    it "attacks a monster rather than displacing it" $ do
      let goblin = mkMonster "Goblin" (V2 5 3) 100 3
          s = movePlayer East (withWorld (\w -> w {monsters = [goblin]}) baseState)
      position (player s) `shouldBe` V2 4 3
      map mHealth (monsters (currentWorld s)) `shouldSatisfy` onlyBetween 94 96

    it "updates the visible area after moving" $ do
      let s = movePlayer East baseState
      visibleAt (V2 4 3) (currentWorld s) `shouldBe` True

  describe "combat" $ do
    let goblin = mkMonster "Goblin" (V2 5 3) 100 3
        withGoblin m = withWorld (\w -> w {monsters = [m]}) baseState

    it "damages the monster by about the player's attack" $
      map mHealth (monsters (currentWorld (combat (withGoblin goblin) goblin True)))
        `shouldSatisfy` onlyBetween 94 96

    it "lets the monster counterattack for about its attack minus resistance" $
      health (player (combat (withGoblin goblin) goblin True)) `shouldSatisfy` onlyBetween 17 19 . pure

    it "never deals negative damage to the player" $ do
      let feeble = mkMonster "Kitten" (V2 5 3) 100 0
      health (player (combat (withGoblin feeble) feeble True)) `shouldBe` 20

    it "fires at the monster on the tile even when handed a stale copy" $ do
      let bow = (mkItem "Bow" Range 1 (V2 0 0)) {iUses = Just 2}
          stale = goblin {mHealth = 1}
          s = executeRangedAttack
                (withPlayer (\p -> p {inventory = [bow]}) (withGoblin goblin)) stale bow
      -- 5 attack + 1 bonus - (100 `div` 10) resistance = 0 damage, and the
      -- goblin survives; using the stale copy it would have been defeated.
      map mHealth (monsters (currentWorld s)) `shouldBe` [100]
      corpses (currentWorld s) `shouldBe` []

    it "does nothing when the ranged target is already gone" $ do
      let bow = (mkItem "Bow" Range 20 (V2 0 0)) {iUses = Just 2}
          s = executeRangedAttack (withPlayer (\p -> p {inventory = [bow]}) baseState) goblin bow
      map iUses (inventory (player s)) `shouldBe` [Just 2]
      message s `shouldBe` []

    it "does not defeat bystanders that happen to be at zero health" $ do
      let rat = mkMonster "Rat" (V2 5 3) 3 1
          ghost = mkMonster "Ghost" (V2 2 3) 0 1
          bow = (mkItem "Bow" Range 20 (V2 0 0)) {iUses = Just 2}
          s0 = withPlayer (\p -> p {inventory = [bow]})
                 (withWorld (\w -> w {monsters = [rat, ghost]}) baseState)
          s = executeRangedAttack s0 rat bow
      map mName (monsters (currentWorld s)) `shouldBe` ["Ghost"]
      xp (player s) `shouldBe` 10

    it "spends a charge of the ranged item and records a corpse" $ do
      let bow = (mkItem "Bow" Range 20 (V2 0 0)) {iUses = Just 2}
          rat = mkMonster "Rat" (V2 5 3) 3 1
          s = executeRangedAttack
                (withPlayer (\p -> p {inventory = [bow]}) (withGoblin rat)) rat bow
      map iUses (inventory (player s)) `shouldBe` [Just 1]
      corpses (currentWorld s) `shouldBe` [V2 5 3]

    it "removes a defeated monster and awards its XP" $ do
      let rat = mkMonster "Rat" (V2 5 3) 3 1
          s = combat (withGoblin rat) rat True
      monsters (currentWorld s) `shouldBe` []
      xp (player s) `shouldBe` 10

    -- Messages for things that did not happen used to be added as empty
    -- strings, taking up lines the message pane could have shown.
    it "writes no blank lines to the log" $ do
      let rat = mkMonster "Rat" (V2 5 3) 3 1
      message (combat (withGoblin goblin) goblin True) `shouldSatisfy` notElem ""
      message (combat (withGoblin rat) rat True) `shouldSatisfy` notElem ""

    it "records a corpse where the monster fell" $ do
      let rat = mkMonster "Rat" (V2 5 3) 3 1
          s = combat (withGoblin rat) rat True
      corpses (currentWorld s) `shouldBe` [V2 5 3]

    it "records no corpse while the monster survives" $
      corpses (currentWorld (combat (withGoblin goblin) goblin True)) `shouldBe` []

    it "leaves a staircase usable when a monster dies on it" $ do
      let stairsMap =
            [ "#####"
            , "#..>#"
            , "#####"
            ]
          rat = mkMonster "Rat" (V2 3 1) 3 1
          s0 = withWorld (\w -> w {monsters = [rat]}) (mkState (mkWorld stairsMap) (V2 2 1))
          s = combat s0 rat True
          onStairs = withPlayer (\p -> p {position = V2 3 1}) s
      tileAt (V2 3 1) (currentWorld s) `shouldBe` DownStair
      corpses (currentWorld s) `shouldBe` [V2 3 1]
      latest (goDown onStairs) `shouldSatisfy` ("bottom level" `isInfixOf`)

    it "does not record the same corpse tile twice" $ do
      let rat n = mkMonster n (V2 5 3) 3 1
          s = combat (combat (withGoblin (rat "Rat")) (rat "Rat") True) (rat "Mouse") True
      corpses (currentWorld s) `shouldBe` [V2 5 3]

    it "sets gameOver when the player's health reaches zero" $ do
      let brute = mkMonster "Brute" (V2 5 3) 100 30
          s = combat (withPlayer (\p -> p {health = 5}) (withGoblin brute)) brute True
      health (player s) `shouldBe` 0
      gameOver s `shouldBe` True

    -- The target is resolved from the world by position, so a caller holding
    -- an out-of-date copy of the monster still hits it.
    it "hits the monster on the tile even when handed a stale copy" $ do
      let hurt = goblin {mHealth = 40} -- no longer matches the world's copy
          s = combat (withGoblin goblin) hurt True
      map mHealth (monsters (currentWorld s)) `shouldSatisfy` onlyBetween 94 96

    it "keeps landing hits when the same stale copy is reused" $ do
      let rat = mkMonster "Rat" (V2 5 3) 12 1
          s = iterate (\st -> combat st rat True) (withGoblin rat) !! 3
      monsters (currentWorld s) `shouldBe` []
      corpses (currentWorld s) `shouldBe` [V2 5 3]

    it "does nothing when the monster is already gone" $ do
      let s = combat baseState goblin True
      health (player s) `shouldBe` 20
      message s `shouldBe` []

    it "reports the damage the world's monster deals, not the stale copy's" $ do
      let stale = goblin {mAttack = 99}
          s = combat (withGoblin goblin) stale True
      -- 20 less about (3 - 1), nowhere near what the stale 99 would do
      health (player s) `shouldSatisfy` onlyBetween 17 19 . pure

    it "ignores inactive monsters entirely" $ do
      let template = goblin {mInactive = True}
          s = combat (withGoblin template) template True
      health (player s) `shouldBe` 20
      map mHealth (monsters (currentWorld s)) `shouldBe` [100]

  describe "rolling damage" $ do
    let roll base seed = fst (rollDamage base (mkStdGen seed))
        rolls base seed n = take n (unfoldr (Just . rollDamage base) (mkStdGen seed))

    it "does nothing when the attack cannot get through" $
      map (`roll` 1) [0, -1, -50] `shouldBe` [0, 0, 0]

    it "stays within a quarter either side of the attack" $
      property $ \(Positive base) (NonNegative seed) ->
        let spread = max 1 (base `div` 4)
         in roll base seed >= max 1 (base - spread)
              && roll base seed <= base + spread

    it "averages out at the attack itself, leaving the balance alone" $ do
      let samples = rolls 20 7 4000
          mean = fromIntegral (sum samples) / fromIntegral (length samples) :: Double
      mean `shouldSatisfy` \m -> abs (m - 20) < 0.5

    it "does not keep rolling the same number" $
      length (nub (rolls 20 7 50)) `shouldSatisfy` (> 3)

    it "gives a different sequence from a different seed" $
      rolls 20 1 20 `shouldSatisfy` (/= rolls 20 2 20)

    it "always does at least a point when the attack gets through" $
      map (roll 1) [1 .. 50] `shouldSatisfy` all (>= 1)

  describe "the generator" $ do
    it "advances when a blow is struck" $ do
      let start = withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 5 3) 100 3]}) baseState
          after' = combat start (mkMonster "Goblin" (V2 5 3) 100 3) True
      show (rng after') `shouldSatisfy` (/= show (rng start))

    it "advances when an arrow is loosed" $ do
      let bow = (mkItem "Bow" Range 3 (V2 0 0)) {iUses = Just 5}
          -- Tough enough to survive, weak enough that the shot gets through:
          -- a monster resists a tenth of its own health.
          target = mkMonster "Goblin" (V2 5 3) 30 3
          start = withPlayer (\p -> p {inventory = [bow]})
                    (withWorld (\w -> w {monsters = [target]}) baseState)
          after' = executeRangedAttack start target bow
      show (rng after') `shouldSatisfy` (/= show (rng start))

    it "does not advance when nothing is struck" $
      show (rng (combat baseState (mkMonster "Ghost" (V2 5 3) 1 1) True))
        `shouldBe` show (rng baseState)

  describe "remembering what has been beaten" $ do
    let rat = mkMonster "Rat" (V2 5 3) 3 1
        withRat = withWorld (\w -> w {monsters = [rat]}) baseState

    it "notes nothing before a fight" $
      defeatedMonsters baseState `shouldBe` []

    it "notes a monster cut down in melee" $
      defeatedMonsters (combat withRat rat True) `shouldBe` ["Rat"]

    it "notes nothing while the monster is still standing" $ do
      let troll = mkMonster "Troll" (V2 5 3) 500 1
      defeatedMonsters (combat (withWorld (\w -> w {monsters = [troll]}) baseState) troll True)
        `shouldBe` []

    it "notes a monster shot at range" $ do
      let bow = (mkItem "Bow" Range 20 (V2 0 0)) {iUses = Just 2}
          s = executeRangedAttack (withPlayer (\p -> p {inventory = [bow]}) withRat) rat bow
      defeatedMonsters s `shouldBe` ["Rat"]

    it "notes everything burned by a firestorm" $ do
      let scroll = mkSpecial "Scroll" Firestorm 40
          lit w = w {visibility = replicate 7 (replicate 9 True)}
          crowd = withPlayer (\p -> p {inventory = [scroll]})
                    (withWorld (lit . (\w -> w {monsters =
                      [mkMonster "Goblin" (V2 1 1) 5 1, mkMonster "Rat" (V2 2 2) 5 1]})) baseState)
      defeatedMonsters (useItem scroll crowd) `shouldSatisfy` \ns ->
        null (["Goblin", "Rat"] \\ ns)

    it "records a name only once" $ do
      let two n = mkMonster n (V2 5 3) 3 1
          s = combat (combat withRat rat True) (two "Rat") True
      defeatedMonsters s `shouldBe` ["Rat"]

    -- The point of recording defeats rather than asking whether any monster
    -- of that name is alive: a boss waits as a spawn template, so "none
    -- alive" would be true before it ever appeared.
    it "does not count a boss that has yet to be called up" $ do
      let sleeping = (mkMonster "Dungeon Lord" (V2 1 1) 99 9) {mInactive = True}
          waiting = withWorld (\w -> w {monsters = [sleeping]}) baseState
      evalTriggerCondition (MonsterDefeated "Dungeon Lord") waiting `shouldBe` False

    it "fires a trigger once the boss is beaten" $ do
      let boss = mkMonster "Dungeon Lord" (V2 5 3) 3 9
          reward = mkTrigger (MonsterDefeated "Dungeon Lord") [DisplayMessage "The vault grinds open."] False
          arena = withWorld (\w -> w {monsters = [boss], triggers = [reward]}) baseState
      latest (processTriggers arena) `shouldSatisfy` (not . ("vault" `isInfixOf`))
      latest (processTriggers (combat arena boss True))
        `shouldSatisfy` ("The vault grinds open." `isInfixOf`)

  describe "levelUp" $ do
    it "does nothing below the next threshold" $ do
      let (p, msgs) = levelUp ((mkPlayer (V2 0 0)) {xp = 99}) testXPLevels
      playerXPLevel p `shouldBe` 1
      msgs `shouldBe` []

    it "raises level, stats and health on reaching a threshold" $ do
      let (p, msgs) = levelUp ((mkPlayer (V2 0 0)) {xp = 100}) testXPLevels
      playerXPLevel p `shouldBe` 2
      baseAttack p `shouldBe` 8
      baseResistance p `shouldBe` 2
      health p `shouldBe` 40
      length msgs `shouldBe` 4

    it "re-applies equipment bonuses after levelling up" $ do
      let sword = mkItem "Sword" Weapon 7 (V2 0 0)
          (p, _) = levelUp ((mkPlayer (V2 0 0)) {xp = 100, equippedWeapon = Just sword}) testXPLevels
      attack p `shouldBe` 15

    it "skips straight to the highest level reached" $ do
      let (p, _) = levelUp ((mkPlayer (V2 0 0)) {xp = 250}) testXPLevels
      playerXPLevel p `shouldBe` 2

  describe "pickUpItem" $ do
    let sword = mkItem "Sword" Weapon 4 (V2 4 3)
        withFloorItem i = withWorld (\w -> w {items = [i]}) baseState

    it "moves the item on the player's tile into the inventory" $ do
      let s = pickUpItem (withFloorItem sword)
      inventory (player s) `shouldBe` [sword]
      items (currentWorld s) `shouldBe` []

    it "reports an empty tile" $ do
      let s = pickUpItem baseState
      inventory (player s) `shouldBe` []
      latest s `shouldSatisfy` ("nothing to pick up" `isInfixOf`)

    it "ignores an item that has not been spawned yet" $ do
      let s = pickUpItem (withFloorItem sword {iInactive = True})
      inventory (player s) `shouldBe` []

    it "stacks items that carry a use count" $ do
      let potion n pos = (mkItem "Potion" Healing 5 pos) {iUses = Just n}
          s = pickUpItem
                . withPlayer (\p -> p {inventory = [potion 2 (V2 0 0)]})
                $ withFloorItem (potion 3 (V2 4 3))
      map iUses (inventory (player s)) `shouldBe` [Just 5]
      items (currentWorld s) `shouldBe` []

    it "refuses to pick up when the inventory is full" $ do
      let junk i = mkItem ("Junk " ++ show i) Special 0 (V2 0 0)
          s = pickUpItem
                . withPlayer (\p -> p {inventory = map junk [1 .. maxInventorySize]})
                $ withFloorItem sword
      length (inventory (player s)) `shouldBe` maxInventorySize
      items (currentWorld s) `shouldBe` [sword]
      latest s `shouldSatisfy` ("full" `isInfixOf`)

  describe "the help" $ do
    let press n = iterate (handleMovementInternal (Just '?')) baseState !! n

    it "is closed to begin with" $
      legendPage baseState `shouldBe` 0

    it "steps through every page and then closes" $
      map (legendPage . press) [0 .. length helpPages + 1]
        `shouldBe` ([0 .. length helpPages] ++ [0])

    it "documents every key the game responds to" $ do
      let documented = unlines (concatMap snd helpPages)
      mapM_
        (\k -> documented `shouldSatisfy` (k `isInfixOf`))
        [ "w or k", "s or j", "a or h", "d or l"
        , "<", ">", "g", "u", "x"
        , "a b c ...", "Esc", ":", "Enter", "Backspace"
        , ":q", ":restart", ":heal", ":super"
        ]

    it "gives every page a title" $
      map fst helpPages `shouldSatisfy` notElem ""

  describe "the item chooser" $ do
    let holding = withPlayer (\p -> p {inventory = [mkItem "Sword" Weapon 4 (V2 0 0)]}) baseState

    it "opens when the player asks to use an item" $ do
      let s = promptUseItem holding
      inventoryMode s `shouldBe` Just UseMode
      commandMode s `shouldBe` True

    it "opens in drop mode when the player asks to drop one" $
      inventoryMode (promptDropItem holding) `shouldBe` Just DropMode

    it "does not open when there is nothing to choose from" $
      inventoryMode (promptUseItem baseState) `shouldBe` Nothing

    it "closes on escape" $ do
      let opened = promptUseItem holding
          escaped = handleCommandInputInternal Nothing True opened opened
      inventoryMode escaped `shouldBe` Nothing
      commandMode escaped `shouldBe` False

    it "closes once an item has been chosen" $ do
      let opened = promptUseItem holding
          chosen = handleCommandInputInternal (Just 'a') False opened opened
      inventoryMode chosen `shouldBe` Nothing

  describe "dropItem" $ do
    let sword = mkItem "Sword" Weapon 4 (V2 0 0)
        holding = withPlayer (\p -> p {inventory = [sword]}) baseState

    it "puts the item back on the player's tile" $ do
      let s = dropItem sword holding
      inventory (player s) `shouldBe` []
      map iPosition (items (currentWorld s)) `shouldBe` [V2 4 3]

    it "refuses when the tile already holds an item" $ do
      let occupied = withWorld (\w -> w {items = [mkItem "Shield" Armor 2 (V2 4 3)]}) holding
          s = dropItem sword occupied
      inventory (player s) `shouldBe` [sword]
      latest s `shouldSatisfy` ("occupied" `isInfixOf`)

  describe "useItem" $ do
    it "heals, capped at the maximum for the player's XP level" $ do
      let potion = (mkItem "Potion" Healing 50 (V2 0 0)) {iUses = Just 1}
          s = useItem potion (withPlayer (\p -> p {health = 5, inventory = [potion]}) baseState)
      health (player s) `shouldBe` 20

    it "spends one use and discards a depleted item" $ do
      let potion = (mkItem "Potion" Healing 3 (V2 0 0)) {iUses = Just 1}
          s = useItem potion (withPlayer (\p -> p {health = 5, inventory = [potion]}) baseState)
      inventory (player s) `shouldBe` []

    it "keeps an item that still has uses left" $ do
      let potion = (mkItem "Potion" Healing 3 (V2 0 0)) {iUses = Just 2}
          s = useItem potion (withPlayer (\p -> p {health = 5, inventory = [potion]}) baseState)
      map iUses (inventory (player s)) `shouldBe` [Just 1]

    -- An item with no use count is never spent. That is right for equipment,
    -- and Game.State.validateItemUses stops a consumable being loaded this way.
    it "never spends an item that declares no use count" $ do
      let potion = mkItem "Endless Potion" Healing 3 (V2 0 0)
          s = useItem potion (withPlayer (\p -> p {health = 5, inventory = [potion]}) baseState)
      health (player s) `shouldBe` 8
      inventory (player s) `shouldBe` [potion]

    it "equips a weapon and adds its bonus to the effective attack" $ do
      let sword = mkItem "Sword" Weapon 4 (V2 0 0)
          s = useItem sword (withPlayer (\p -> p {inventory = [sword]}) baseState)
      equippedWeapon (player s) `shouldBe` Just sword
      attack (player s) `shouldBe` 9

    it "unequips a weapon that is already equipped" $ do
      let sword = mkItem "Sword" Weapon 4 (V2 0 0)
          equipped = useItem sword (withPlayer (\p -> p {inventory = [sword]}) baseState)
          s = useItem sword equipped
      equippedWeapon (player s) `shouldBe` Nothing
      attack (player s) `shouldBe` 5

    it "equips armor and adds its bonus to the effective resistance" $ do
      let shield = mkItem "Shield" Armor 3 (V2 0 0)
          s = useItem shield (withPlayer (\p -> p {inventory = [shield]}) baseState)
      resistance (player s) `shouldBe` 4

    it "enters aiming mode for a ranged item" $ do
      let bow = mkItem "Bow" Range 6 (V2 0 0)
          s = useItem bow (withPlayer (\p -> p {inventory = [bow]}) baseState)
      fmap aimingItem (aimingState s) `shouldBe` Just bow
      commandMode s `shouldBe` True

    it "unlocks an adjacent door with the matching key" $ do
      let key = (mkItem "Iron Key" Key 0 (V2 0 0)) {iUses = Just 1}
          s = useItem key
                . withPlayer (\p -> p {inventory = [key]})
                $ withWorld (\w -> w {doors = [mkDoor (V2 5 3) True "Iron Key"]}) baseState
      map deLocked (doors (currentWorld s)) `shouldBe` [False]
      inventory (player s) `shouldBe` []

    it "rejects a key that does not fit the lock" $ do
      let key = (mkItem "Brass Key" Key 0 (V2 0 0)) {iUses = Just 1}
          s = useItem key
                . withPlayer (\p -> p {inventory = [key]})
                $ withWorld (\w -> w {doors = [mkDoor (V2 5 3) True "Iron Key"]}) baseState
      map deLocked (doors (currentWorld s)) `shouldBe` [True]
      latest s `shouldSatisfy` ("does not fit" `isInfixOf`)

    it "reports when there is no door to unlock" $ do
      let key = (mkItem "Iron Key" Key 0 (V2 0 0)) {iUses = Just 1}
          s = useItem key (withPlayer (\p -> p {inventory = [key]}) baseState)
      latest s `shouldSatisfy` ("no door nearby" `isInfixOf`)

  describe "Special items" $ do
    let carryingOne itm = withPlayer (\p -> p {inventory = [itm]}) baseState
        packOf s = map iName (inventory (player s))

    describe "using one that fires once" $ do
      it "Empower raises attack for good, and is spent" $ do
        let tome = mkSpecial "Tome" Empower 5
            s = useItem tome (carryingOne tome)
        baseAttack (player s) `shouldBe` 10
        attack (player s) `shouldBe` 10
        packOf s `shouldBe` []

      it "Empower keeps the weapon bonus on top" $ do
        let tome = mkSpecial "Tome" Empower 5
            sword = mkItem "Sword" Weapon 4 (V2 0 0)
            armed = withPlayer (\p -> p {inventory = [tome], equippedWeapon = Just sword}) baseState
            s = useItem tome armed
        baseAttack (player s) `shouldBe` 10
        attack (player s) `shouldBe` 14

      it "Fortify raises resistance for good, and is spent" $ do
        let amulet = mkSpecial "Amulet" Fortify 10
            s = useItem amulet (carryingOne amulet)
        baseResistance (player s) `shouldBe` 11
        packOf s `shouldBe` []

      it "Reveal maps the floor" $ do
        let scroll = mkSpecial "Scroll" Reveal 0
            s = useItem scroll (carryingOne scroll)
        concat (discovered (currentWorld s)) `shouldSatisfy` and
        packOf s `shouldBe` []

      it "Blink moves the player onto a floor tile" $ do
        let potion = mkSpecial "Potion" Blink 0
            s = useItem potion (carryingOne potion)
        position (player s) `shouldSatisfy` (/= V2 4 3)
        tileAt (position (player s)) (currentWorld s) `shouldBe` Floor
        packOf s `shouldBe` []

      it "Blink lights up wherever it drops the player" $ do
        let potion = mkSpecial "Potion" Blink 0
            s = useItem potion (carryingOne potion)
        visibleAt (position (player s)) (currentWorld s) `shouldBe` True

      it "Vanish hides the player for a while, and is spent" $ do
        let potion = mkSpecial "Potion" Vanish 10
            s = useItem potion (carryingOne potion)
        hiddenTurns s `shouldBe` 10
        packOf s `shouldBe` []

      it "Keepsake does nothing and is never spent" $ do
        let coin = mkSpecial "Gold Coin" Keepsake 0
            s = useItem coin (carryingOne coin)
        packOf s `shouldBe` ["Gold Coin"]
        latest s `shouldSatisfy` ("not something you can use" `isInfixOf`)

    describe "Firestorm" $ do
      let scroll = mkSpecial "Scroll of Fireball" Firestorm 40
          lit w = w {visibility = replicate 7 (replicate 9 True)}
          withMonsters ms =
            withPlayer (\p -> p {inventory = [scroll]})
              (withWorld (lit . (\w -> w {monsters = ms})) baseState)

      it "kills everything in sight and pays out the XP" $ do
        let s = useItem scroll (withMonsters
                  [mkMonster "Goblin" (V2 1 1) 10 2, mkMonster "Rat" (V2 2 2) 5 1])
        monsters (currentWorld s) `shouldBe` []
        xp (player s) `shouldBe` 20
        length (corpses (currentWorld s)) `shouldBe` 2

      it "leaves a monster that survives the blast standing" $ do
        let s = useItem scroll (withMonsters [mkMonster "Troll" (V2 1 1) 500 2])
        map mName (monsters (currentWorld s)) `shouldBe` ["Troll"]
        map mHealth (monsters (currentWorld s)) `shouldSatisfy` onlyBetween 440 470

      it "spares anything the player cannot see" $ do
        let dark = withPlayer (\p -> p {inventory = [scroll]})
                     (withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 1 1) 10 2]}) baseState)
            s = useItem scroll dark
        map mName (monsters (currentWorld s)) `shouldBe` ["Goblin"]

      it "is spent even when there is nothing to burn" $ do
        let s = useItem scroll (withMonsters [])
        packOf s `shouldBe` []
        latest s `shouldSatisfy` ("nothing in particular" `isInfixOf`)

    describe "the ones that work while carried" $ do
      it "Regenerate heals a little each turn" $ do
        let ring = mkSpecial "Ring of Vitality" Regenerate 2
            hurt = withPlayer (\p -> p {health = 10, inventory = [ring]}) baseState
        health (player (processTurn hurt)) `shouldBe` 12

      it "Regenerate does not heal past the maximum" $ do
        let ring = mkSpecial "Ring of Vitality" Regenerate 2
            whole = withPlayer (\p -> p {health = 20, inventory = [ring]}) baseState
        health (player (processTurn whole)) `shouldBe` 20

      it "Regenerate is not spent by using it" $ do
        let ring = mkSpecial "Ring of Vitality" Regenerate 2
            s = useItem ring (carryingOne ring)
        packOf s `shouldBe` ["Ring of Vitality"]

      it "Lifesteal gives back a share of the damage dealt" $ do
        let ring = mkSpecial "Bloodstone Ring" Lifesteal 100
            goblin = mkMonster "Goblin" (V2 5 3) 100 3
            hurt = withPlayer (\p -> p {health = 5, inventory = [ring]})
                     (withWorld (\w -> w {monsters = [goblin]}) baseState)
            s = combat hurt goblin True
        -- about five back from the blow, less about two from the counterblow
        health (player s) `shouldSatisfy` onlyBetween 6 9 . pure

      it "Revive catches a killing blow once, and burns up doing it" $ do
        let feather = mkSpecial "Phoenix Feather" Revive 0
            brute = mkMonster "Brute" (V2 5 3) 100 30
            doomed = withPlayer (\p -> p {health = 1, inventory = [feather]})
                       (withWorld (\w -> w {monsters = [brute]}) baseState)
            s = combat doomed brute True
        gameOver s `shouldBe` False
        health (player s) `shouldBe` 20
        packOf s `shouldBe` []
        latest s `shouldSatisfy` ("standing again" `isInfixOf`)

      it "without the feather the same blow is fatal" $ do
        let brute = mkMonster "Brute" (V2 5 3) 100 30
            doomed = withPlayer (\p -> p {health = 1}) (withWorld (\w -> w {monsters = [brute]}) baseState)
        gameOver (combat doomed brute True) `shouldBe` True

    describe "being hidden" $ do
      let goblin = mkMonster "Goblin" (V2 5 3) 100 5
          seen = withWorld (\w -> w {monsters = [goblin]}) baseState
          unseen = seen {hiddenTurns = 3}

      it "stops monsters swinging at the player" $
        health (player (monstersAttack (monstersAttack unseen))) `shouldBe` 20

      it "stops monsters closing in" $ do
        let far = (withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 7 3) 10 2]}) baseState)
                    {hiddenTurns = 3}
        map mPosition (monsters (currentWorld (moveMonsters far))) `shouldBe` [V2 7 3]

      it "wears off a turn at a time" $
        map hiddenTurns (take 5 (iterate processTurn unseen)) `shouldBe` [3, 2, 1, 0, 0]

      it "lets them find the player again once it has" $ do
        let worn = iterate processTurn unseen !! 4
        health (player (monstersAttack (monstersAttack worn))) `shouldSatisfy` (< 20)

  describe "stairs" $ do
    -- Level 0 has a down staircase under the player, level 1 an up staircase.
    let downStairs = mkWorld ["###", "#>#", "###"]
        upStairs   = mkWorld ["###", "#<#", "###"]
        twoLevels  = (mkState downStairs (V2 1 1)) {levels = [downStairs, upStairs]}

    it "descends from a down staircase" $ do
      let s = goDown twoLevels
      currentLevel s `shouldBe` 1
      latest s `shouldSatisfy` ("descend" `isInfixOf`)

    it "ascends from an up staircase" $ do
      let s = goUp (goDown twoLevels)
      currentLevel s `shouldBe` 0
      latest s `shouldSatisfy` ("ascend" `isInfixOf`)

    it "will not descend past the bottom level" $
      latest (goDown (mkState downStairs (V2 1 1)))
        `shouldSatisfy` ("bottom level" `isInfixOf`)

    it "will not ascend past the top level" $
      latest (goUp (mkState upStairs (V2 1 1)))
        `shouldSatisfy` ("top level" `isInfixOf`)

    it "remembers the furthest down the player went" $ do
      let descended = goDown twoLevels
      deepestLevel descended `shouldBe` 1
      -- coming back up does not undo having been there
      deepestLevel (goUp descended) `shouldBe` 1

    it "starts out at the top" $
      deepestLevel twoLevels `shouldBe` 0

    it "refuses to use stairs that are not there" $
      latest (goDown baseState) `shouldSatisfy` ("No stairs" `isInfixOf`)

  describe "shafts" $ do
    -- Level 0 is the way out, with a down staircase to level 1; level 1 has
    -- the matching up staircase and a shaft off in the corner.
    let upper = mkWorld ["####", "#.>#", "####"]
        lower = mkWorld ["####", "#^<#", "####"]
        rope = mkSpecial "Coil of Rope" Escape 0
        below = (mkState upper (V2 2 1)) {levels = [upper, lower], currentLevel = 1}
        onShaft = withPlayer (\pl -> pl {position = V2 1 1, inventory = [rope]}) below
        surface = withPlayer (\pl -> pl {position = V2 1 1, inventory = [rope]})
                    (mkState (mkWorld ["####", "#^>#", "####"]) (V2 1 1))

    it "climbs to the floor above, landing by the stairs down" $ do
      let s = useSpecial rope onShaft
      currentLevel s `shouldBe` 0
      position (player s) `shouldBe` V2 2 1
      latest s `shouldSatisfy` ("climb" `isInfixOf`)

    it "spends the rope on the way up" $
      inventory (player (useSpecial rope onShaft)) `shouldBe` []

    it "refuses anywhere there is no shaft" $ do
      let elsewhere = withPlayer (\pl -> pl {position = V2 2 1}) onShaft
          s = useSpecial rope elsewhere
      currentLevel s `shouldBe` 1
      latest s `shouldSatisfy` ("nothing overhead" `isInfixOf`)
      inventory (player s) `shouldBe` [rope]

    -- What the shaft on the first floor is for: there is no floor above it,
    -- so climbing it is leaving, and that ends the run.
    it "ends the run when climbed from the top floor" $ do
      let s = useSpecial rope surface
      gameWon s `shouldBe` True
      latest s `shouldSatisfy` ("open air" `isInfixOf`)

    -- The old ways out fired the moment they were stood on, which ended runs
    -- that were only passing through.
    it "does nothing at all to someone merely standing on it" $ do
      let s = movePlayer West (withPlayer (\pl -> pl {position = V2 2 1}) onShaft)
      gameWon s `shouldBe` False
      currentLevel s `shouldBe` 1
      latest s `shouldSatisfy` ("Daylight" `isInfixOf`)

    it "is walked over like any other floor" $
      position (player (movePlayer West (withPlayer (\pl -> pl {position = V2 2 1}) onShaft)))
        `shouldBe` V2 1 1

  describe "monster movement" $ do
    let withMonsterAt pos playerPos =
          withWorld (\w -> w {monsters = [mkMonster "Goblin" pos 10 2]})
            (mkState (mkWorld openMap) playerPos)

    it "steps towards the player when within range" $
      map mPosition (monsters (currentWorld (moveMonsters (withMonsterAt (V2 7 3) (V2 4 3)))))
        `shouldBe` [V2 6 3]

    it "stays put when the player is out of range" $
      map mPosition (monsters (currentWorld (moveMonsters (withMonsterAt (V2 7 5) (V2 1 1)))))
        `shouldBe` [V2 7 5]

    it "stays put when already adjacent to the player" $
      map mPosition (monsters (currentWorld (moveMonsters (withMonsterAt (V2 5 3) (V2 4 3)))))
        `shouldBe` [V2 5 3]

    it "does not move two monsters onto the same tile" $ do
      let s = moveMonsters
                ( withWorld
                    (\w -> w {monsters = [mkMonster "A" (V2 6 3) 10 2, mkMonster "B" (V2 7 3) 10 2]})
                    baseState
                )
          positions = map mPosition (monsters (currentWorld s))
      length positions `shouldBe` 2
      length (nub positions) `shouldBe` 2

    it "leaves inactive spawn templates where they are" $ do
      let template = (mkMonster "Goblin" (V2 7 3) 10 2) {mInactive = True}
          s = moveMonsters (withWorld (\w -> w {monsters = [template]}) baseState)
      map mPosition (monsters (currentWorld s)) `shouldBe` [V2 7 3]

  describe "finding a way to the player" $ do
    -- A wall between the two. Straight-line movement walks into it and
    -- stays there for as long as the player stands still.
    --
    --   01234
    -- 0 #####
    -- 1 #M#P#
    -- 2 #...#
    -- 3 #####
    let detourMap = ["#####", "#.#.#", "#...#", "#####"]
        chase grid monsterAt playerAt tweak =
          let st = tweak (withWorld (\w -> w {monsters = [mkMonster "Goblin" monsterAt 10 2]})
                            (mkState (mkWorld grid) playerAt))
           in map mPosition (monsters (currentWorld (moveMonsters st)))

    it "goes around a wall rather than pressing against it" $
      chase detourMap (V2 1 1) (V2 3 1) id `shouldBe` [V2 1 2]

    it "keeps going around on the following turn" $ do
      let st = withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 1 1) 10 2]})
                 (mkState (mkWorld detourMap) (V2 3 1))
          twice = moveMonsters (moveMonsters st)
      map mPosition (monsters (currentWorld twice)) `shouldBe` [V2 2 2]

    it "still walks straight at the player across open ground" $
      chase ["#####", "#...#", "#####"] (V2 3 1) (V2 1 1) id `shouldBe` [V2 2 1]

    it "stays put when the player is sealed away" $ do
      let sealed = ["#####", "#.#.#", "#.#.#", "#####"]
      chase sealed (V2 1 1) (V2 3 1) id `shouldBe` [V2 1 1]

    it "does not come through a locked door" $
      chase ["#####", "#...#", "#####"] (V2 1 1) (V2 3 1)
        (withWorld (\w -> w {doors = [mkDoor (V2 2 1) True "Iron Key"]}))
        `shouldBe` [V2 1 1]

    it "comes through a door that is unlocked" $
      chase ["#####", "#...#", "#####"] (V2 1 1) (V2 3 1)
        (withWorld (\w -> w {doors = [mkDoor (V2 2 1) False "Iron Key"]}))
        `shouldBe` [V2 2 1]

    it "ignores a player who is close by but a long way round" $ do
      -- Two tiles apart, but the way between them is far longer than the
      -- distance a monster will bother to travel.
      let horseshoe =
            [ "#########"
            , "#.......#"
            , "#.#####.#"
            , "#.#M#P#.#"
            , "#.#.#.#.#"
            , "#.......#"
            , "#########"
            ]
      chase horseshoe (V2 3 3) (V2 5 3) id `shouldBe` [V2 3 3]

  describe "monstersAttack" $ do
    let goblin = mkMonster "Goblin" (V2 5 3) 100 5
        s0 = withWorld (\w -> w {monsters = [goblin]}) baseState

    it "makes an adjacent monster wait one turn before its first attack" $ do
      let s1 = monstersAttack s0
      health (player s1) `shouldBe` 20
      map mAttackWait (monsters (currentWorld s1)) `shouldBe` [False]

    it "attacks on the following turn" $ do
      let s2 = monstersAttack (monstersAttack s0)
      health (player s2) `shouldSatisfy` onlyBetween 15 17 . pure

    it "leaves a monster that is not adjacent alone" $ do
      let far = withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 7 3) 100 5]}) baseState
      health (player (monstersAttack (monstersAttack far))) `shouldBe` 20

  describe "executeAction" $ do
    it "SpawnItem activates a pre-placed inactive item" $ do
      let hidden = (mkItem "Dark Sword" Weapon 9 (V2 6 3)) {iInactive = True}
          s = executeAction (withWorld (\w -> w {items = [hidden]}) baseState)
                            (SpawnItem "Dark Sword" (V2 6 3))
      map iInactive (items (currentWorld s)) `shouldBe` [False]

    it "SpawnMonster brings an inactive template to life at a position" $ do
      let template = (mkMonster "Dragon" (V2 0 0) 50 9) {mInactive = True}
          s = executeAction (withWorld (\w -> w {monsters = [template]}) baseState)
                            (SpawnMonster "Dragon" (V2 6 3))
          live = filter (not . mInactive) (monsters (currentWorld s))
      map mPosition live `shouldBe` [V2 6 3]

    it "SpawnMonster reports a missing template" $
      latest (executeAction baseState (SpawnMonster "Dragon" (V2 6 3)))
        `shouldSatisfy` ("No inactive monster template" `isInfixOf`)

    it "UnlockDoor unlocks the door at a position" $ do
      let s = executeAction (withWorld (\w -> w {doors = [mkDoor (V2 5 3) True "Iron Key"]}) baseState)
                            (UnlockDoor (V2 5 3))
      map deLocked (doors (currentWorld s)) `shouldBe` [False]

    it "ShiftTile changes the tile and records an override" $ do
      let s = executeAction baseState (ShiftTile (V2 0 0) Floor)
      tileAt (V2 0 0) (currentWorld s) `shouldBe` Floor
      tileOverrides (currentWorld s) `shouldBe` [(V2 0 0, Floor)]

    it "TransportPlayer moves the player and refreshes what they can see" $ do
      let s = executeAction baseState (TransportPlayer (V2 1 1))
      position (player s) `shouldBe` V2 1 1
      visibleAt (V2 1 1) (currentWorld s) `shouldBe` True

    it "ConsumeItem removes the item from the inventory" $ do
      let coin = mkItem "Gold Coin" Special 0 (V2 0 0)
          s = executeAction (withPlayer (\p -> p {inventory = [coin]}) baseState)
                            (ConsumeItem "Gold Coin")
      inventory (player s) `shouldBe` []

    it "AddToInventory moves an inactive item into the inventory" $ do
      let reward = (mkItem "Mithril Shield" Armor 8 (V2 0 0)) {iInactive = True}
          s = executeAction (withWorld (\w -> w {items = [reward]}) baseState)
                            (AddToInventory "Mithril Shield")
      map iName (inventory (player s)) `shouldBe` ["Mithril Shield"]
      items (currentWorld s) `shouldBe` []

    it "HarmPlayer springs a trap" $ do
      let s = executeAction (withPlayer (\p -> p {health = 20}) baseState) (HarmPlayer 7)
      health (player s) `shouldBe` 13
      gameOver s `shouldBe` False
      latest s `shouldSatisfy` ("7 damage" `isInfixOf`)

    it "a trap can kill" $ do
      let s = executeAction (withPlayer (\p -> p {health = 5}) baseState) (HarmPlayer 40)
      health (player s) `shouldBe` 0
      gameOver s `shouldBe` True
      message s `shouldSatisfy` any ("died" `isInfixOf`)

    it "HealPlayer mends, but not past the maximum" $ do
      let hurt = withPlayer (\p -> p {health = 5}) baseState
      health (player (executeAction hurt (HealPlayer 7))) `shouldBe` 12
      health (player (executeAction hurt (HealPlayer 500))) `shouldBe` 20

    it "SetGameWon wins the game" $
      gameWon (executeAction baseState SetGameWon) `shouldBe` True

  describe "processTriggers" $ do
    -- The player starts on (4, 3) in the fixture room.
    let atStart = AtPosition (V2 4 3)
        elsewhere = AtPosition (V2 1 1)

    it "fires a matching trigger and then discards it" $ do
      let s = processTriggers
                (withWorld (\w -> w {triggers = [mkTrigger atStart [SetGameWon] False]}) baseState)
      gameWon s `shouldBe` True
      triggers (currentWorld s) `shouldBe` []

    it "keeps a recurring trigger for the next turn" $ do
      let t = mkTrigger atStart [SetGameWon] True
          s = processTriggers (withWorld (\w -> w {triggers = [t]}) baseState)
      gameWon s `shouldBe` True
      triggers (currentWorld s) `shouldBe` [t]

    it "leaves a trigger whose condition does not hold" $ do
      let t = mkTrigger elsewhere [SetGameWon] False
          s = processTriggers (withWorld (\w -> w {triggers = [t]}) baseState)
      gameWon s `shouldBe` False
      triggers (currentWorld s) `shouldBe` [t]

    it "fires a trigger that needs both a position and items" $ do
      let t = mkTrigger (AtPositionWithItems (V2 4 3) ["Gold Coin"]) [SetGameWon] False
          without = withWorld (\w -> w {triggers = [t]}) baseState
          with = withPlayer (\p -> p {inventory = [mkItem "Gold Coin" Special 0 (V2 0 0)]}) without
      gameWon (processTriggers without) `shouldBe` False
      gameWon (processTriggers with) `shouldBe` True

    it "runs every action of a trigger in order" $ do
      let t = mkTrigger atStart [DisplayMessage "first", DisplayMessage "second"] False
          s = processTriggers (withWorld (\w -> w {triggers = [t]}) baseState)
      take 2 (message s) `shouldBe` ["second", "first"]

  describe "getVisibleMonsters" $ do
    let seen = withWorld (updateVisibleRoom . addMonsters)
        addMonsters w = w {monsters = [mkMonster "Goblin" (V2 5 3) 10 2]}
        updateVisibleRoom w = w {visibility = replicate 7 (replicate 9 True)}

    it "labels visible monsters from 'a' upwards" $
      map fst (getVisibleMonsters (seen baseState)) `shouldBe` "a"

    -- The map draws its letters from the same list, so 'a' means the same
    -- monster in both places.
    it "ignores inactive spawn templates" $ do
      let template = (mkMonster "Dragon" (V2 2 3) 50 9) {mInactive = True}
          s = withWorld (\w -> w {monsters = template : monsters w}) (seen baseState)
      map (mName . snd) (getVisibleMonsters s) `shouldBe` ["Goblin"]

    it "agrees with the level's own visible-monster list" $ do
      let template = (mkMonster "Dragon" (V2 2 3) 50 9) {mInactive = True}
          s = withWorld (\w -> w {monsters = template : monsters w}) (seen baseState)
      getVisibleMonsters s `shouldBe` visibleMonsters (currentWorld s)

  describe "calculateRangedDamage" $
    it "adds the item bonus and subtracts a tenth of the target's health" $
      calculateRangedDamage (mkPlayer (V2 0 0)) (mkMonster "Goblin" (V2 0 0) 50 2)
        (mkItem "Bow" Range 6 (V2 0 0))
        `shouldBe` 6 -- 5 attack + 6 bonus - 5 resistance

  describe "processTurn" $ do
    it "trims the message log" $ do
      let noisy = baseState {message = map show [1 .. 30 :: Int]}
      length (message (processTurn noisy)) `shouldBe` maxLogMessages

    it "cycles the turn clock so NPCs step every third turn" $
      map keyPressCount (take 4 (iterate processTurn baseState))
        `shouldBe` [0, 1, 2, 0]

  describe "what counts as a turn" $ do
    let withMonster =
          withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 7 3) 10 2]}) baseState
        monsterPositions st = map mPosition (monsters (currentWorld st))

    it "spends a turn on a move, so monsters act" $ do
      let s = handleMovementInternal (Just 'd') withMonster
      monsterPositions s `shouldBe` [V2 6 3]
      keyPressCount s `shouldBe` 1

    it "spends no turn opening the help" $ do
      let s = handleMovementInternal (Just '?') withMonster
      legendPage s `shouldBe` 1
      monsterPositions s `shouldBe` [V2 7 3]
      keyPressCount s `shouldBe` 0

    it "spends no turn opening command mode" $ do
      let s = handleMovementInternal (Just ':') withMonster
      commandMode s `shouldBe` True
      commandBuffer s `shouldBe` ":"
      monsterPositions s `shouldBe` [V2 7 3]
      keyPressCount s `shouldBe` 0

    it "still opens the help once the game is over" $ do
      let s = handleMovementInternal (Just '?') withMonster {gameOver = True}
      legendPage s `shouldBe` 1
      monsterPositions s `shouldBe` [V2 7 3]
