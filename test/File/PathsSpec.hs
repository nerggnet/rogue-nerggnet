-- test/File/PathsSpec.hs
--
-- Which dungeon a run belongs to decides where everything it leaves behind
-- goes. Two dungeons must not share a scoreboard: the scores would not be
-- comparable, which is the one thing the scoreboard is for.
module File.PathsSpec (spec) where

import Data.List (isPrefixOf, nub)
import File.Paths
import Test.Hspec

spec :: Spec
spec = describe "where a dungeon's files go" $ do
  let shipped = pathsFor defaultWorldFile
      other = pathsFor "caves.json"
      allOf p = [saveFile p, scoresFile p, gravesFile p, replayDir p]

  -- An existing scoreboard is still an existing scoreboard.
  it "leaves the dungeon that ships with the names it has always had" $
    allOf shipped `shouldBe` ["save.json", "scores.json", "graves.json", "replays"]

  it "gives any other dungeon a corner of its own" $
    allOf other `shouldSatisfy` all ("packs/caves/" `isPrefixOf`)

  it "keeps two dungeons from sharing anything" $
    (allOf shipped ++ allOf other) `shouldSatisfy` \paths -> length (nub paths) == length paths

  it "names the corner after the dungeon, not the path to it" $
    saveFile (pathsFor "somewhere/else/caves.json") `shouldBe` saveFile other

  it "remembers which dungeon it is for" $ do
    worldFile shipped `shouldBe` defaultWorldFile
    worldFile other `shouldBe` "caves.json"
