-- test/Game/StateSpec.hs
module Game.StateSpec (spec) where

import Data.Aeson (decode, encode)
import Game.GridUtils (gridLookup)
import Game.State
import Game.Types
import Linear.V2 (V2 (..))
import Test.Hspec
import Test.QuickCheck
import qualified File.Types as FT

import Fixtures

-- Points in a range small enough that lines stay short.
smallPoint :: Gen (V2 Int)
smallPoint = V2 <$> choose (-20, 20) <*> choose (-20, 20)

-- Chebyshev distance: 1 for an orthogonal or diagonal step.
step :: V2 Int -> V2 Int -> Int
step (V2 x1 y1) (V2 x2 y2) = max (abs (x2 - x1)) (abs (y2 - y1))

spec :: Spec
spec = do
  describe "charToTile / tileToChar" $ do
    it "round-trips every tile that has a map character" $
      map (charToTile . tileToChar) [Wall, Floor, Door, UpStair, DownStair, Start]
        `shouldBe` [Wall, Floor, Door, UpStair, DownStair, Start]

    it "treats an unrecognised character as floor" $
      property $ \c -> c `notElem` "#.+<>S" ==> charToTile c === Floor

  describe "manhattanDistance" $ do
    it "is zero exactly when the points are equal" $
      forAll ((,) <$> smallPoint <*> smallPoint) $ \(a, b) ->
        (manhattanDistance a b == 0) === (a == b)

    it "is symmetric" $
      forAll ((,) <$> smallPoint <*> smallPoint) $ \(a, b) ->
        manhattanDistance a b === manhattanDistance b a

    it "satisfies the triangle inequality" $
      forAll ((,,) <$> smallPoint <*> smallPoint <*> smallPoint) $ \(a, b, c) ->
        manhattanDistance a c <= manhattanDistance a b + manhattanDistance b c

  describe "bresenhamLine" $ do
    -- The lines are bounded so that a runaway implementation fails the test
    -- rather than hanging the suite.
    let line a b = take 200 (bresenhamLine a b)

    it "is a single point when source and destination coincide" $
      bresenhamLine (V2 3 4) (V2 3 4) `shouldBe` [V2 3 4]

    it "walks a straight horizontal line" $
      bresenhamLine (V2 0 0) (V2 3 0)
        `shouldBe` [V2 0 0, V2 1 0, V2 2 0, V2 3 0]

    it "walks a straight vertical line" $
      bresenhamLine (V2 0 0) (V2 0 3)
        `shouldBe` [V2 0 0, V2 0 1, V2 0 2, V2 0 3]

    it "walks a clean diagonal" $
      bresenhamLine (V2 0 0) (V2 3 3)
        `shouldBe` [V2 0 0, V2 1 1, V2 2 2, V2 3 3]

    it "starts at the source and ends at the destination" $
      forAll ((,) <$> smallPoint <*> smallPoint) $ \(a, b) ->
        case line a b of
          []           -> counterexample "empty line" False
          l@(start : _) -> start === a .&&. last l === b

    it "never jumps more than one tile at a time" $
      forAll ((,) <$> smallPoint <*> smallPoint) $ \(a, b) ->
        let l = line a b in all (== 1) (zipWith step l (drop 1 l))

  describe "updateVisibility" $ do
    let world = mkWorld openMap
        seen radius pos = updateVisibility (mkPlayer pos) radius world

    it "makes the player's own tile visible" $
      visibleAt (V2 4 3) (seen 2 (V2 4 3)) `shouldBe` True

    it "makes tiles within the radius visible" $
      visibleAt (V2 5 3) (seen 2 (V2 4 3)) `shouldBe` True

    it "leaves tiles beyond the radius invisible" $
      visibleAt (V2 7 3) (seen 2 (V2 4 3)) `shouldBe` False

    it "marks everything it can see as discovered" $
      discoveredAt (V2 5 3) (seen 2 (V2 4 3)) `shouldBe` True

    it "keeps tiles discovered after they leave view" $ do
      let afterFirst  = updateVisibility (mkPlayer (V2 1 1)) 1 world
          afterSecond = updateVisibility (mkPlayer (V2 7 5)) 1 afterFirst
      visibleAt (V2 1 1) afterSecond `shouldBe` False
      discoveredAt (V2 1 1) afterSecond `shouldBe` True

    it "sees a wall but not what is behind it" $ do
      let v = updateVisibility (mkPlayer (V2 1 3)) 5 (mkWorld wallMap)
      visibleAt (V2 3 3) v `shouldBe` True  -- floor in front of the wall
      visibleAt (V2 4 3) v `shouldBe` True  -- the wall itself
      visibleAt (V2 5 3) v `shouldBe` False -- behind the wall

    it "is blocked by a locked door" $ do
      let locked = world {doors = [mkDoor (V2 6 3) True "Iron Key"]}
      visibleAt (V2 8 3) (updateVisibility (mkPlayer (V2 4 3)) 5 locked)
        `shouldBe` False

    it "is not blocked by an unlocked door" $ do
      let unlocked = world {doors = [mkDoor (V2 6 3) False "Iron Key"]}
      visibleAt (V2 8 3) (updateVisibility (mkPlayer (V2 4 3)) 5 unlocked)
        `shouldBe` True

  describe "newGame" $ do
    let validGrid = ["#####", "#S..#", "#####"]
        withItems is lvl = lvl {FT.items = is}

    it "builds a game from a minimal configuration" $ do
      st <- shouldSucceed (newGame testGen (jsonConfig [jsonLevel validGrid]))
      st.player.position `shouldBe` V2 1 1
      length (levels st) `shouldBe` 1

    it "reports a configuration with no XP levels" $
      newGame testGen (jsonConfig [jsonLevel validGrid]) {FT.xpLevels = []}
        `shouldReport` "xpLevels"

    it "reports a configuration with no levels" $
      newGame testGen (jsonConfig []) `shouldReport` "levels"

    it "reports a first level with nowhere to start" $
      newGame testGen (jsonConfig [jsonLevel ["#####", "#...#", "#####"]])
        `shouldReport` "S"

    it "reports a ragged map and names the rows that differ" $ do
      let r = newGame testGen (jsonConfig [jsonLevel ["#####", "#S.#", "#####"]])
      r `shouldReport` "ragged"
      r `shouldReport` "row(s) 1"

    it "says which level a problem came from" $
      newGame testGen (jsonConfig [jsonLevel validGrid, jsonLevel ["###", "##"]])
        `shouldReport` "level 1"

    it "reports problems from every level, not just the first" $ do
      let broken name = withItems [jsonItemOf name "Sandwich"] (jsonLevel validGrid)
          r = newGame testGen (jsonConfig [broken "First", broken "Second"])
      r `shouldReport` "level 0"
      r `shouldReport` "level 1"
      r `shouldReport` "First"
      r `shouldReport` "Second"
      either length (const 0) r `shouldBe` 2

    it "points at the item inside the level" $
      newGame testGen (jsonConfig [withItems [jsonItemOf "Lamp" "Healing"] (jsonLevel validGrid)])
        `shouldReport` "level 0: item \"Lamp\": a Healing item must declare"

  describe "gridLookup" $ do
    let grid = [[1 :: Int, 2, 3], [4, 5, 6]] -- 3 wide, 2 tall

    it "reads a cell by (x, y)" $ do
      gridLookup grid (V2 2 1) `shouldBe` Just 6
      gridLookup grid (V2 0 0) `shouldBe` Just 1

    it "returns Nothing past the right or bottom edge" $ do
      gridLookup grid (V2 3 0) `shouldBe` Nothing
      gridLookup grid (V2 0 2) `shouldBe` Nothing

    it "returns Nothing for negative coordinates" $ do
      gridLookup grid (V2 (-1) 0) `shouldBe` Nothing
      gridLookup grid (V2 0 (-1)) `shouldBe` Nothing

    it "returns Nothing for an empty grid" $
      gridLookup ([] :: [[Int]]) (V2 0 0) `shouldBe` Nothing

  describe "visibleMonsters" $ do
    -- The map and the ranged-targeting logic both read this list, so the
    -- letters they show can no longer disagree.
    let lit w = w {visibility = replicate 7 (replicate 9 True)}
        withMonsters ms = lit ((mkWorld openMap) {monsters = ms})

    it "labels visible monsters from 'a' upwards" $
      map fst (visibleMonsters (withMonsters
        [mkMonster "Goblin" (V2 2 3) 10 2, mkMonster "Rat" (V2 5 3) 5 1]))
        `shouldBe` "ab"

    it "ignores inactive spawn templates" $
      map (mName . snd) (visibleMonsters (withMonsters
        [ (mkMonster "Dragon" (V2 2 3) 50 9) {mInactive = True}
        , mkMonster "Goblin" (V2 5 3) 10 2
        ]))
        `shouldBe` ["Goblin"]

    it "ignores monsters standing in the dark" $
      map (mName . snd) (visibleMonsters
        ((mkWorld openMap) {monsters = [mkMonster "Goblin" (V2 5 3) 10 2]}))
        `shouldBe` []

    it "does not crash on a monster outside the map" $
      visibleMonsters (withMonsters [mkMonster "Ghost" (V2 100 100) 1 1])
        `shouldBe` []

  describe "isVisibleAt" $ do
    let lit = (mkWorld openMap) {visibility = replicate 7 (replicate 9 True)}

    it "is True for a lit tile" $ isVisibleAt lit (V2 4 3) `shouldBe` True
    it "is False outside the map" $ isVisibleAt lit (V2 100 100) `shouldBe` False
    it "is False for a negative position" $ isVisibleAt lit (V2 (-1) 0) `shouldBe` False

  describe "transformItem" $ do
    let jsonItem n cat uses =
          FT.JSONItem
            { FT.itemName = n
            , FT.itemPosition = (1, 2)
            , FT.itemDescription = ""
            , FT.itemCategory = cat
            , FT.itemEffectValue = 5
            , FT.itemHidden = False
            , FT.itemInactive = False
            , FT.itemUses = uses
            , FT.itemEffect = Nothing
            }

    it "reads a well-formed item" $ do
      i <- shouldSucceed $ transformItem (jsonItem "Health Potion" "Healing" (Just 3))
      iCategory i `shouldBe` Healing
      iUses i `shouldBe` Just 3
      iPosition i `shouldBe` V2 1 2

    it "allows equipment to omit a use count" $
      traverse (\cat -> iUses <$> shouldSucceed (transformItem (jsonItem "Thing" cat Nothing)))
        ["Weapon", "Armor"]
        `shouldReturn` [Nothing, Nothing]

    it "reads the effect of a Special item" $ do
      i <- shouldSucceed
        (transformItem (jsonItem "Phoenix Feather" "Special" Nothing) {FT.itemEffect = Just "Revive"})
      iEffect i `shouldBe` Just Revive

    it "reports a Special item with no effect, since it would do nothing" $
      transformItem (jsonItem "Odd Trinket" "Special" Nothing)
        `shouldReport` "must declare an \"itemEffect\""

    it "names an unknown effect and lists the valid ones" $ do
      let r = transformItem (jsonItem "Thing" "Special" Nothing) {FT.itemEffect = Just "Levitate"}
      r `shouldReport` "Levitate"
      r `shouldReport` "Firestorm"

    it "reports an effect on a category that already has behaviour" $
      transformItem (jsonItem "Sword" "Weapon" Nothing) {FT.itemEffect = Just "Empower"}
        `shouldReport` "only a Special item"

    it "reports a consumable that omits its use count" $
      mapM_
        (\cat -> transformItem (jsonItem "Thing" cat Nothing) `shouldReport` "itemUses")
        ["Healing", "Key", "Range"]

    it "names the category that needs a use count" $
      transformItem (jsonItem "Greater Health Potion" "Healing" Nothing)
        `shouldReport` "Healing item must declare"

    it "names an unknown category and lists the valid ones" $ do
      let r = transformItem (jsonItem "Thing" "Sandwich" Nothing)
      r `shouldReport` "Sandwich"
      r `shouldReport` "Healing"

  describe "findStartingPosition" $ do
    it "finds the S tile" $
      findStartingPosition (mkWorld openMap) `shouldBe` Just (V2 4 3)

    it "finds nothing when the map has no S tile" $
      findStartingPosition (mkWorld ["###", "...", "###"]) `shouldBe` Nothing

  describe "the discovered grid" $
    it "round-trips through its coordinate list" $
      forAll (vectorOf 5 (vector 7)) $ \grid ->
        coordsToGrid (gridToCoords grid) 5 7 === grid

  describe "maxHealth" $ do
    it "reads the table by level number rather than by position" $ do
      -- The same levels, listed in the wrong order: indexing would read level 3.
      let shuffled = baseState {xpLevels = reverse testXPLevels}
      maxHealth (withPlayer (\p -> p {playerXPLevel = 1}) shuffled) `shouldBe` 20
      maxHealth (withPlayer (\p -> p {playerXPLevel = 2}) shuffled) `shouldBe` 40
      maxHealth (withPlayer (\p -> p {playerXPLevel = 3}) shuffled) `shouldBe` 60

    it "handles a table that skips level numbers" $ do
      let sparse = baseState {xpLevels = [XPLevel 1 0 20 5 1, XPLevel 7 100 99 9 9]}
      maxHealth (withPlayer (\p -> p {playerXPLevel = 7}) sparse) `shouldBe` 99

    it "falls back to current health when the level is missing" $
      maxHealth (withPlayer (\p -> p {playerXPLevel = 99, health = 7}) baseState)
        `shouldBe` 7

  describe "allMonstersDefeated" $ do
    it "holds when the level has no monsters" $
      allMonstersDefeated baseState `shouldBe` True

    it "does not hold while an active monster remains" $
      allMonstersDefeated
        (withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 1 1) 5 1]}) baseState)
        `shouldBe` False

    it "ignores inactive spawn templates" $
      allMonstersDefeated
        ( withWorld
            (\w -> w {monsters = [(mkMonster "Goblin" (V2 1 1) 5 1) {mInactive = True}]})
            baseState
        )
        `shouldBe` True

  describe "transformJSONTrigger" $ do
    let conditionFrom t = triggerCondition <$> shouldSucceed (transformJSONTrigger t)

    it "builds a position condition" $
      conditionFrom baseJSONTrigger {FT.triggerType = "position", FT.target = Just (5, 6)}
        `shouldReturn` AtPosition (V2 5 6)

    it "builds a posAndItems condition" $
      conditionFrom baseJSONTrigger
          { FT.triggerType = "posAndItems"
          , FT.target = Just (49, 14)
          , FT.requiredItems = Just ["Gold Coin", "Magic Ring"]
          }
        `shouldReturn` AtPositionWithItems (V2 49 14) ["Gold Coin", "Magic Ring"]

    it "builds an itemPickup condition" $
      conditionFrom baseJSONTrigger
          {FT.triggerType = "itemPickup", FT.triggerItemName = Just "Gold Coin"}
        `shouldReturn` HasItem "Gold Coin"

    it "builds an npcTalked condition" $
      conditionFrom baseJSONTrigger
          {FT.triggerType = "npcTalked", FT.triggerNpcName = Just "Friendly NPC"}
        `shouldReturn` TalkedToNpc "Friendly NPC"

    it "builds an allMonstersDefeated condition" $
      conditionFrom baseJSONTrigger {FT.triggerType = "allMonstersDefeated"}
        `shouldReturn` AllMonstersDefeated

    it "carries the actions and the recurring flag across" $ do
      t <- shouldSucceed $ transformJSONTrigger baseJSONTrigger
                { FT.triggerType = "position"
                , FT.target = Just (1, 1)
                , FT.recurring = True
                , FT.actions =
                    [ FT.JSONTriggerAction
                        { FT.actionType = "displayMessage"
                        , FT.actionPosition = Nothing
                        , FT.actionItemName = Nothing
                        , FT.actionMonsterName = Nothing
                        , FT.actionTileType = Nothing
                        , FT.actionMessage = Just "Hello"
                        }
                    ]
                }
      triggerActions t `shouldBe` [DisplayMessage "Hello"]
      triggerRecurring t `shouldBe` True

    it "names an unknown trigger type and lists the valid ones" $ do
      let r = transformJSONTrigger baseJSONTrigger {FT.triggerType = "explode"}
      r `shouldReport` "explode"
      r `shouldReport` "allMonstersDefeated"

    it "says which field a position trigger is missing" $
      transformJSONTrigger baseJSONTrigger {FT.triggerType = "position"}
        `shouldReport` "target"

    it "says which fields a posAndItems trigger is missing" $
      transformJSONTrigger baseJSONTrigger
          {FT.triggerType = "posAndItems", FT.target = Just (1, 1)}
        `shouldReport` "requiredItems"

  describe "evalTriggerCondition" $ do
    let carrying ns =
          withPlayer (\p -> p {inventory = map (\n -> mkItem n Special 0 (V2 0 0)) ns}) baseState

    it "AtPosition holds only on that tile" $ do
      evalTriggerCondition (AtPosition (V2 4 3)) baseState `shouldBe` True
      evalTriggerCondition (AtPosition (V2 4 4)) baseState `shouldBe` False

    it "AtPositionWithItems needs the tile and every item" $ do
      let cond = AtPositionWithItems (V2 4 3) ["Gold Coin", "Magic Ring"]
      evalTriggerCondition cond (carrying ["Gold Coin", "Magic Ring"]) `shouldBe` True
      evalTriggerCondition cond (carrying ["Gold Coin"]) `shouldBe` False
      evalTriggerCondition cond (carrying ["Magic Ring", "Gold Coin", "Rope"]) `shouldBe` True

    it "AtPositionWithItems does not hold on the wrong tile" $
      evalTriggerCondition
        (AtPositionWithItems (V2 1 1) ["Gold Coin"])
        (carrying ["Gold Coin"])
        `shouldBe` False

    it "HasItem checks the inventory" $ do
      evalTriggerCondition (HasItem "Gold Coin") (carrying ["Gold Coin"]) `shouldBe` True
      evalTriggerCondition (HasItem "Gold Coin") (carrying ["Silver Coin"]) `shouldBe` False
      evalTriggerCondition (HasItem "Gold Coin") baseState `shouldBe` False

    it "TalkedToNpc checks the last NPC spoken to" $ do
      evalTriggerCondition (TalkedToNpc "Bob") baseState {lastInteractedNpc = Just "Bob"}
        `shouldBe` True
      evalTriggerCondition (TalkedToNpc "Bob") baseState {lastInteractedNpc = Just "Ann"}
        `shouldBe` False
      evalTriggerCondition (TalkedToNpc "Bob") baseState `shouldBe` False

    it "AllMonstersDefeated ignores inactive templates" $ do
      let template = (mkMonster "Dragon" (V2 1 1) 5 1) {mInactive = True}
      evalTriggerCondition AllMonstersDefeated baseState `shouldBe` True
      evalTriggerCondition AllMonstersDefeated
        (withWorld (\w -> w {monsters = [template]}) baseState) `shouldBe` True
      evalTriggerCondition AllMonstersDefeated
        (withWorld (\w -> w {monsters = [mkMonster "Goblin" (V2 1 1) 5 1]}) baseState)
        `shouldBe` False

  describe "trigger serialisation" $ do
    -- Triggers are plain data now, so they survive JSON unchanged instead of
    -- being rendered to prose and parsed back.
    let conditions =
          [ AtPosition (V2 5 6)
          , AtPositionWithItems (V2 49 14) ["Gold Coin", "Magic Ring"]
          , HasItem "Gold Coin"
          , TalkedToNpc "Friendly NPC"
          , AllMonstersDefeated
          ]

    it "round-trips every condition through JSON" $
      mapM_ (\c -> decode (encode c) `shouldBe` Just c) conditions

    it "round-trips a whole trigger through JSON" $ do
      let t = Trigger
                { triggerCondition = AtPositionWithItems (V2 1 2) ["Gold Coin"]
                , triggerActions = [DisplayMessage "Hi", SetGameWon]
                , triggerRecurring = True
                }
      decode (encode t) `shouldBe` Just t

    it "survives item names that used to break the prose format" $ do
      -- Commas, brackets and quotes are just string contents now.
      let awkward = AtPositionWithItems (V2 1 2) ["Rope, 50ft", "Boots [worn]", "\"Lucky\" Coin"]
      decode (encode awkward) `shouldBe` Just awkward

  describe "validateTriggers" $ do
    let itemNamed n =
          FT.JSONItem
            { FT.itemName = n
            , FT.itemPosition = (0, 0)
            , FT.itemDescription = ""
            , FT.itemCategory = "Special"
            , FT.itemEffectValue = 0
            , FT.itemHidden = False
            , FT.itemInactive = False
            , FT.itemUses = Nothing
            , FT.itemEffect = Nothing
            }
        npcNamed n = FT.JSONNPC {FT.npcName = n, FT.npcPosition = (0, 0), FT.npcMessage = ""}
        trigger c = Trigger {triggerCondition = c, triggerActions = [], triggerRecurring = False}

    it "accepts a trigger whose item exists" $
      fmap (map triggerCondition)
        (validateTriggers [trigger (HasItem "Gold Coin")] [itemNamed "Gold Coin"] [])
        `shouldBe` Right [HasItem "Gold Coin"]

    it "names an item the level does not define" $
      validateTriggers [trigger (HasItem "No Such Item")] [] []
        `shouldReport` "No Such Item"

    it "names every missing item of a posAndItems trigger" $ do
      let r = validateTriggers
                [trigger (AtPositionWithItems (V2 0 0) ["Gold Coin", "Ghost Item"])]
                [itemNamed "Gold Coin"]
                []
      r `shouldReport` "Ghost Item"

    it "names an NPC the level does not define" $
      validateTriggers [trigger (TalkedToNpc "Nobody")] [] [npcNamed "Bob"]
        `shouldReport` "Nobody"

    it "reports every bad trigger, not just the first" $ do
      let r = validateTriggers
                [trigger (HasItem "Ghost A"), trigger (TalkedToNpc "Ghost B")] [] []
      r `shouldReport` "Ghost A"
      r `shouldReport` "Ghost B"
      either length (const 0) r `shouldBe` 2

    it "says which trigger is at fault" $
      validateTriggers [trigger AllMonstersDefeated, trigger (HasItem "Ghost")] [] []
        `shouldReport` "trigger 1"

    it "does not look at items for an allMonstersDefeated trigger" $
      fmap (map triggerCondition) (validateTriggers [trigger AllMonstersDefeated] [] [])
        `shouldBe` Right [AllMonstersDefeated]

  describe "transformJSONAction" $ do
    let action t = baseAction {FT.actionType = t}
        baseAction =
          FT.JSONTriggerAction
            { FT.actionType = ""
            , FT.actionPosition = Nothing
            , FT.actionItemName = Nothing
            , FT.actionMonsterName = Nothing
            , FT.actionTileType = Nothing
            , FT.actionMessage = Nothing
            }

    it "builds a spawnItem action" $
      transformJSONAction
        (action "spawnItem") {FT.actionItemName = Just "Sword", FT.actionPosition = Just (1, 2)}
        `shouldBe` Right (SpawnItem "Sword" (V2 1 2))

    it "builds a shiftTile action from a map character" $
      transformJSONAction
        (action "shiftTile") {FT.actionPosition = Just (1, 2), FT.actionTileType = Just '#'}
        `shouldBe` Right (ShiftTile (V2 1 2) Wall)

    it "builds setGameWon" $
      transformJSONAction (action "setGameWon") `shouldBe` Right SetGameWon

    it "names an unknown action type" $
      transformJSONAction (action "explode") `shouldReport` "explode"

    it "says which field a spawnItem action is missing" $
      transformJSONAction (action "spawnItem") {FT.actionItemName = Just "Sword"}
        `shouldReport` "actionPosition"
