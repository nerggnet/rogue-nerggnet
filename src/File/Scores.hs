-- src/File/Scores.hs
--
-- The scoreboard on disk. One line of history per finished run.
module File.Scores
  ( loadScores
  , recordRun
  , timestampNow
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecodeFileStrict, encode)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Game.Score
import Game.Types (GameState, Run)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import qualified Data.ByteString.Lazy as B

-- | How many runs are kept. Enough to be a history, not so many that the
-- file grows without end.
keptRuns :: Int
keptRuns = 100

-- | Every run recorded so far, best first.
--
-- A missing file is an empty scoreboard rather than an error: the first run
-- anyone plays has nothing to compare against, and that is not a fault. A
-- file that will not parse reports why and is treated the same, because
-- losing the history is not a reason to refuse to play.
loadScores :: FilePath -> IO (Either String [Run])
loadScores path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right [])
    else do
      parsed <- eitherDecodeFileStrict path
      pure $ case parsed of
        Left err   -> Left (path ++ ": " ++ err)
        Right runs -> Right (ranked runs)

-- | Write a finished run to the scoreboard, and say where it placed.
--
-- Nothing when the game is not over, so this can be called unconditionally
-- at the end of a session without asking first.
recordRun :: FilePath -> String -> GameState -> IO (Maybe (Run, Int, Int))
recordRun path when state =
  case runOf when state of
    Nothing -> pure Nothing
    Just run -> do
      existing <- fromRight [] <$> loadScores path
      let kept = take keptRuns (ranked (run : existing))
      -- A dungeon of its own keeps its files in a corner of its own, which
      -- may not exist yet. Without this the scoreboard for a pack was
      -- written nowhere, quietly, because the write is guarded.
      written <- try $ do
        createDirectoryIfMissing True (takeDirectory path)
        B.writeFile path (encode kept)
      case written :: Either SomeException () of
        -- A scoreboard that cannot be written must not take the run down
        -- with it. The player still gets told how they did.
        Left _  -> pure (Just (run, 0, length existing + 1))
        Right _ -> pure (Just (run, fromMaybe 0 (placeOf run kept), length kept))

-- | The local date and time, for stamping a run.
timestampNow :: IO String
timestampNow = do
  now <- getCurrentTime
  zone <- getCurrentTimeZone
  pure (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (utcToLocalTime zone now))
