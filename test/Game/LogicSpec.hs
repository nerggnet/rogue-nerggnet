-- test/Game/LogicSpec.hs
module Game.LogicSpec (spec) where

import Data.List (isInfixOf, nub)
import Game.Logic
import Game.State (maxInventorySize)
import Game.Types
import Linear.V2 (V2 (..))
import Test.Hspec

import Fixtures

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

  describe "replace" $ do
    it "swaps only the first matching element" $
      replace 1 9 [1, 2, 1 :: Int] `shouldBe` [9, 2, 1]

    it "leaves a list without a match alone" $
      replace 7 9 [1, 2, 3 :: Int] `shouldBe` [1, 2, 3]

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

    it "talks to an NPC rather than displacing it" $ do
      let s = movePlayer East (withWorld (\w -> w {npcs = [mkNPC "Bob" (V2 5 3)]}) baseState)
      position (player s) `shouldBe` V2 4 3
      lastInteractedNpc s `shouldBe` Just "Bob"
      latest s `shouldSatisfy` ("Bob says: hello" `isInfixOf`)

    it "attacks a monster rather than displacing it" $ do
      let goblin = mkMonster "Goblin" (V2 5 3) 100 3
          s = movePlayer East (withWorld (\w -> w {monsters = [goblin]}) baseState)
      position (player s) `shouldBe` V2 4 3
      map mHealth (monsters (currentWorldOf s)) `shouldBe` [95]

    it "updates the visible area after moving" $ do
      let s = movePlayer East baseState
      visibleAt (V2 4 3) (currentWorldOf s) `shouldBe` True

  describe "combat" $ do
    let goblin = mkMonster "Goblin" (V2 5 3) 100 3
        withGoblin m = withWorld (\w -> w {monsters = [m]}) baseState

    it "damages the monster by the player's effective attack" $
      map mHealth (monsters (currentWorldOf (combat (withGoblin goblin) goblin True)))
        `shouldBe` [95]

    it "lets the monster counterattack for its attack minus resistance" $
      health (player (combat (withGoblin goblin) goblin True)) `shouldBe` 18

    it "never deals negative damage to the player" $ do
      let feeble = mkMonster "Kitten" (V2 5 3) 100 0
      health (player (combat (withGoblin feeble) feeble True)) `shouldBe` 20

    it "removes a defeated monster and awards its XP" $ do
      let rat = mkMonster "Rat" (V2 5 3) 3 1
          s = combat (withGoblin rat) rat True
      monsters (currentWorldOf s) `shouldBe` []
      xp (player s) `shouldBe` 10

    it "leaves a Death tile where the monster fell" $ do
      let rat = mkMonster "Rat" (V2 5 3) 3 1
          s = combat (withGoblin rat) rat True
      tileAt (V2 5 3) (currentWorldOf s) `shouldBe` Death

    -- Known issue: the Death marker overwrites the terrain, so a monster that
    -- dies on a staircase destroys it.
    it "overwrites a staircase when a monster dies on it" $ do
      let stairsMap =
            [ "#####"
            , "#..>#"
            , "#####"
            ]
          rat = mkMonster "Rat" (V2 3 1) 3 1
          s0 = withWorld (\w -> w {monsters = [rat]}) (mkState (mkWorld stairsMap) (V2 2 1))
          s = combat s0 rat True
      tileAt (V2 3 1) (currentWorldOf s) `shouldBe` Death
      latest (goDown (withPlayer (\p -> p {position = V2 3 1}) s))
        `shouldSatisfy` ("No stairs" `isInfixOf`)

    it "sets gameOver when the player's health reaches zero" $ do
      let brute = mkMonster "Brute" (V2 5 3) 100 30
          s = combat (withPlayer (\p -> p {health = 5}) (withGoblin brute)) brute True
      health (player s) `shouldBe` 0
      gameOver s `shouldBe` True

    it "ignores inactive monsters entirely" $ do
      let template = goblin {mInactive = True}
          s = combat (withGoblin template) template True
      health (player s) `shouldBe` 20
      map mHealth (monsters (currentWorldOf s)) `shouldBe` [100]

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
      items (currentWorldOf s) `shouldBe` []

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
      items (currentWorldOf s) `shouldBe` []

    it "refuses to pick up when the inventory is full" $ do
      let junk i = mkItem ("Junk " ++ show i) Special 0 (V2 0 0)
          s = pickUpItem
                . withPlayer (\p -> p {inventory = map junk [1 .. maxInventorySize]})
                $ withFloorItem sword
      length (inventory (player s)) `shouldBe` maxInventorySize
      items (currentWorldOf s) `shouldBe` [sword]
      latest s `shouldSatisfy` ("full" `isInfixOf`)

  describe "dropItem" $ do
    let sword = mkItem "Sword" Weapon 4 (V2 0 0)
        carrying = withPlayer (\p -> p {inventory = [sword]}) baseState

    it "puts the item back on the player's tile" $ do
      let s = dropItem sword carrying
      inventory (player s) `shouldBe` []
      map iPosition (items (currentWorldOf s)) `shouldBe` [V2 4 3]

    it "refuses when the tile already holds an item" $ do
      let occupied = withWorld (\w -> w {items = [mkItem "Shield" Armor 2 (V2 4 3)]}) carrying
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

    -- Known issue: reduceUses maps over a Maybe, so an item with no use count
    -- is never spent. world.json has healing potions and a key like this.
    it "never spends an item whose use count is null" $ do
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
      map deLocked (doors (currentWorldOf s)) `shouldBe` [False]
      inventory (player s) `shouldBe` []

    it "rejects a key that does not fit the lock" $ do
      let key = (mkItem "Brass Key" Key 0 (V2 0 0)) {iUses = Just 1}
          s = useItem key
                . withPlayer (\p -> p {inventory = [key]})
                $ withWorld (\w -> w {doors = [mkDoor (V2 5 3) True "Iron Key"]}) baseState
      map deLocked (doors (currentWorldOf s)) `shouldBe` [True]
      latest s `shouldSatisfy` ("does not fit" `isInfixOf`)

    it "reports when there is no door to unlock" $ do
      let key = (mkItem "Iron Key" Key 0 (V2 0 0)) {iUses = Just 1}
          s = useItem key (withPlayer (\p -> p {inventory = [key]}) baseState)
      latest s `shouldSatisfy` ("no door nearby" `isInfixOf`)

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

    it "refuses to use stairs that are not there" $
      latest (goDown baseState) `shouldSatisfy` ("No stairs" `isInfixOf`)

  describe "monster movement" $ do
    let withMonsterAt pos playerPos =
          withWorld (\w -> w {monsters = [mkMonster "Goblin" pos 10 2]})
            (mkState (mkWorld openMap) playerPos)

    it "steps towards the player when within range" $
      map mPosition (monsters (currentWorldOf (moveMonsters (withMonsterAt (V2 7 3) (V2 4 3)))))
        `shouldBe` [V2 6 3]

    it "stays put when the player is out of range" $
      map mPosition (monsters (currentWorldOf (moveMonsters (withMonsterAt (V2 7 5) (V2 1 1)))))
        `shouldBe` [V2 7 5]

    it "stays put when already adjacent to the player" $
      map mPosition (monsters (currentWorldOf (moveMonsters (withMonsterAt (V2 5 3) (V2 4 3)))))
        `shouldBe` [V2 5 3]

    it "does not move two monsters onto the same tile" $ do
      let s = moveMonsters
                ( withWorld
                    (\w -> w {monsters = [mkMonster "A" (V2 6 3) 10 2, mkMonster "B" (V2 7 3) 10 2]})
                    baseState
                )
          positions = map mPosition (monsters (currentWorldOf s))
      length positions `shouldBe` 2
      length (nub positions) `shouldBe` 2

    it "leaves inactive spawn templates where they are" $ do
      let template = (mkMonster "Goblin" (V2 7 3) 10 2) {mInactive = True}
          s = moveMonsters (withWorld (\w -> w {monsters = [template]}) baseState)
      map mPosition (monsters (currentWorldOf s)) `shouldBe` [V2 7 3]

  describe "monstersAttack" $ do
    let goblin = mkMonster "Goblin" (V2 5 3) 100 5
        s0 = withWorld (\w -> w {monsters = [goblin]}) baseState

    it "makes an adjacent monster wait one turn before its first attack" $ do
      let s1 = monstersAttack s0
      health (player s1) `shouldBe` 20
      map mAttackWait (monsters (currentWorldOf s1)) `shouldBe` [False]

    it "attacks on the following turn" $ do
      let s2 = monstersAttack (monstersAttack s0)
      health (player s2) `shouldBe` 16

    it "leaves a monster that is not adjacent alone" $ do
      let far = withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 7 3) 100 5]}) baseState
      health (player (monstersAttack (monstersAttack far))) `shouldBe` 20

  describe "executeAction" $ do
    it "SpawnItem activates a pre-placed inactive item" $ do
      let hidden = (mkItem "Dark Sword" Weapon 9 (V2 6 3)) {iInactive = True}
          s = executeAction (withWorld (\w -> w {items = [hidden]}) baseState)
                            (SpawnItem "Dark Sword" (V2 6 3))
      map iInactive (items (currentWorldOf s)) `shouldBe` [False]

    it "SpawnMonster brings an inactive template to life at a position" $ do
      let template = (mkMonster "Dragon" (V2 0 0) 50 9) {mInactive = True}
          s = executeAction (withWorld (\w -> w {monsters = [template]}) baseState)
                            (SpawnMonster "Dragon" (V2 6 3))
          live = filter (not . mInactive) (monsters (currentWorldOf s))
      map mPosition live `shouldBe` [V2 6 3]

    it "SpawnMonster reports a missing template" $
      latest (executeAction baseState (SpawnMonster "Dragon" (V2 6 3)))
        `shouldSatisfy` ("No inactive monster template" `isInfixOf`)

    it "UnlockDoor unlocks the door at a position" $ do
      let s = executeAction (withWorld (\w -> w {doors = [mkDoor (V2 5 3) True "Iron Key"]}) baseState)
                            (UnlockDoor (V2 5 3))
      map deLocked (doors (currentWorldOf s)) `shouldBe` [False]

    it "ShiftTile changes the tile and records an override" $ do
      let s = executeAction baseState (ShiftTile (V2 0 0) Floor)
      tileAt (V2 0 0) (currentWorldOf s) `shouldBe` Floor
      tileOverrides (currentWorldOf s) `shouldBe` [(V2 0 0, Floor)]

    it "TransportPlayer moves the player and refreshes what they can see" $ do
      let s = executeAction baseState (TransportPlayer (V2 1 1))
      position (player s) `shouldBe` V2 1 1
      visibleAt (V2 1 1) (currentWorldOf s) `shouldBe` True

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
      items (currentWorldOf s) `shouldBe` []

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
      triggers (currentWorldOf s) `shouldBe` []

    it "keeps a recurring trigger for the next turn" $ do
      let t = mkTrigger atStart [SetGameWon] True
          s = processTriggers (withWorld (\w -> w {triggers = [t]}) baseState)
      gameWon s `shouldBe` True
      triggers (currentWorldOf s) `shouldBe` [t]

    it "leaves a trigger whose condition does not hold" $ do
      let t = mkTrigger elsewhere [SetGameWon] False
          s = processTriggers (withWorld (\w -> w {triggers = [t]}) baseState)
      gameWon s `shouldBe` False
      triggers (currentWorldOf s) `shouldBe` [t]

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

    -- Known issue: UI.Draw filters inactive monsters out before assigning
    -- letters and this does not, so the two can disagree about what 'a' means.
    it "also labels inactive spawn templates" $ do
      let template = (mkMonster "Dragon" (V2 2 3) 50 9) {mInactive = True}
          s = withWorld (\w -> w {monsters = template : monsters w}) (seen baseState)
      map (mName . snd) (getVisibleMonsters s) `shouldBe` ["Dragon", "Goblin"]

  describe "calculateRangedDamage" $
    it "adds the item bonus and subtracts a tenth of the target's health" $
      calculateRangedDamage (mkPlayer (V2 0 0)) (mkMonster "Goblin" (V2 0 0) 50 2)
        (mkItem "Bow" Range 6 (V2 0 0))
        `shouldBe` 6 -- 5 attack + 6 bonus - 5 resistance

  describe "processTurn" $
    it "trims the message log to ten lines" $ do
      let noisy = baseState {message = map show [1 .. 30 :: Int]}
      length (message (processTurn noisy)) `shouldBe` 10
