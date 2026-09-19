-- test/Game/StateSpec.hs
module Game.StateSpec (spec) where

import Control.Exception (evaluate)
import Data.Maybe (isNothing)
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

    it "cannot round-trip Death, which is not a map character" $
      charToTile (tileToChar Death) `shouldBe` Floor

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

  describe "findStartingPosition" $ do
    it "finds the S tile" $
      findStartingPosition (mkWorld openMap) `shouldBe` V2 4 3

    it "falls back to the origin when the map has no S tile" $
      findStartingPosition (mkWorld ["###", "...", "###"]) `shouldBe` V2 0 0

  describe "the discovered grid" $
    it "round-trips through its coordinate list" $
      forAll (vectorOf 5 (vector 7)) $ \grid ->
        coordsToGrid (gridToCoords grid) 5 7 === grid

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

  describe "trigger serialisation" $ do
    -- Triggers hold a GameState -> Bool, so they are saved by rendering an
    -- English description and parsing it back on load. These specs pin down
    -- which shapes survive that round trip.
    let roundTrip = toRuntimeTrigger . transformToSerializableTrigger

    it "round-trips a position trigger" $ do
      let t = roundTrip baseJSONTrigger {FT.triggerType = "position", FT.target = Just (5, 6)}
      triggerCondition t (mkState (mkWorld openMap) (V2 5 6)) `shouldBe` True
      triggerCondition t (mkState (mkWorld openMap) (V2 5 7)) `shouldBe` False

    it "round-trips an itemPickup trigger" $ do
      let t = roundTrip baseJSONTrigger
                { FT.triggerType = "itemPickup"
                , FT.triggerItemName = Just "Gold Coin"
                }
          carrying n = withPlayer (\p -> p {inventory = [mkItem n Special 0 (V2 0 0)]}) baseState
      triggerCondition t (carrying "Gold Coin") `shouldBe` True
      triggerCondition t (carrying "Silver Coin") `shouldBe` False

    it "round-trips an npcTalked trigger" $ do
      let t = roundTrip baseJSONTrigger
                { FT.triggerType = "npcTalked"
                , FT.triggerNpcName = Just "Friendly NPC"
                }
      triggerCondition t baseState {lastInteractedNpc = Just "Friendly NPC"} `shouldBe` True
      triggerCondition t baseState {lastInteractedNpc = Just "Someone Else"} `shouldBe` False
      triggerCondition t baseState `shouldBe` False

    it "round-trips an allMonstersDefeated trigger" $ do
      let t = roundTrip baseJSONTrigger {FT.triggerType = "allMonstersDefeated"}
      triggerCondition t baseState `shouldBe` True

    it "preserves the actions and the recurring flag" $ do
      let t = roundTrip baseJSONTrigger
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

    -- Known gap: transformToSerializableTrigger has no "posAndItems" case.
    -- The only posAndItems trigger in world.json is on the first level, which
    -- keeps a differently-built trigger list, so this is currently masked.
    it "cannot yet round-trip a posAndItems trigger" $ do
      let jt = baseJSONTrigger
                 { FT.triggerType = "posAndItems"
                 , FT.target = Just (49, 14)
                 , FT.requiredItems = Just ["Gold Coin", "Magic Ring"]
                 }
      description (transformToSerializableTrigger jt) `shouldBe` "Unknown trigger type"
      evaluate (triggerCondition (roundTrip jt)) `shouldThrow` anyErrorCall

    -- Known gap: the two description generators disagree for this trigger
    -- type, and only one of the two spellings can be parsed back.
    it "has two spellings of the allMonstersDefeated description" $ do
      let jt = baseJSONTrigger {FT.triggerType = "allMonstersDefeated"}
      description (transformToSerializableTrigger jt)
        `shouldBe` "Trigger when all monsters are defeated"
      triggerDescription (transformJSONTrigger jt)
        `shouldBe` "Trigger when all monsters on the level are defeated"
      isNothing (parseTriggerType (triggerDescription (transformJSONTrigger jt)))
        `shouldBe` True

  describe "validateTriggers" $
    -- Known gap: the guards test for "itemPickup" / "npcTalked" / "posAndItems",
    -- but descriptions spell those out as prose, so nothing is ever rejected.
    it "accepts a trigger that refers to an item the level does not have" $ do
      let jt = baseJSONTrigger
                 { FT.triggerType = "itemPickup"
                 , FT.triggerItemName = Just "No Such Item"
                 }
          validated = validateTriggers [transformJSONTrigger jt] [] []
      map triggerDescription validated
        `shouldBe` ["Item pickup trigger for Just \"No Such Item\""]

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
        `shouldBe` SpawnItem "Sword" (V2 1 2)

    it "builds a shiftTile action from a map character" $
      transformJSONAction
        (action "shiftTile") {FT.actionPosition = Just (1, 2), FT.actionTileType = Just '#'}
        `shouldBe` ShiftTile (V2 1 2) Wall

    it "builds setGameWon" $
      transformJSONAction (action "setGameWon") `shouldBe` SetGameWon

    it "rejects an unknown action type" $
      evaluate (transformJSONAction (action "explode")) `shouldThrow` anyErrorCall

    it "rejects a spawnItem action that is missing its position" $
      evaluate (transformJSONAction (action "spawnItem") {FT.actionItemName = Just "Sword"})
        `shouldThrow` anyErrorCall
