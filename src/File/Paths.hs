-- src/File/Paths.hs
--
-- Where a dungeon's files live.
--
-- The game is an engine and world.json is the course it ships with. Another
-- dungeon is another course, and everything a run leaves behind -- the save,
-- the scoreboard, the dead, the recordings -- belongs to the dungeon it was
-- played in. Scores from two different dungeons are not comparable, and a
-- body from one has no business in the other.
--
-- The dungeon that ships keeps the file names it has always had, so an
-- existing scoreboard is still an existing scoreboard. Anything else is
-- given a corner of its own under packs/.
module File.Paths
  ( Paths (..)
  , defaultWorldFile
  , pathsFor
  ) where

import System.FilePath (dropExtension, takeFileName, (</>))

defaultWorldFile :: FilePath
defaultWorldFile = "world.json"

data Paths = Paths
  { worldFile  :: FilePath
  , saveFile   :: FilePath
  , scoresFile :: FilePath
  , gravesFile :: FilePath
  , replayDir  :: FilePath
  }
  deriving (Eq, Show)

pathsFor :: FilePath -> Paths
pathsFor world
  | world == defaultWorldFile = Paths
      { worldFile = world
      , saveFile = "save.json"
      , scoresFile = "scores.json"
      , gravesFile = "graves.json"
      , replayDir = "replays"
      }
  | otherwise = Paths
      { worldFile = world
      , saveFile = home </> "save.json"
      , scoresFile = home </> "scores.json"
      , gravesFile = home </> "graves.json"
      , replayDir = home </> "replays"
      }
  where
    home = "packs" </> dropExtension (takeFileName world)
