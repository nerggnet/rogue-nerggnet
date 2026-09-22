-- src/File/Replays.hs
--
-- Runs on disk, and the fingerprint of the dungeon they were played on.
module File.Replays
  ( defaultReplayDir
  , worldDigest
  , saveReplay
  , loadReplay
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecodeFileStrict, encode)
import File.MapIO (defaultWorldFile)
import Game.Replay
import Game.Types (GameState)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import qualified Data.ByteString.Lazy as B

defaultReplayDir :: FilePath
defaultReplayDir = "replays"

-- | The fingerprint of the dungeon as it stands on disk.
--
-- Read as bytes rather than as a parsed world, so that reformatting the
-- file counts as changing it. A replay is keys pressed at particular
-- monsters standing in particular places; anything that moves them makes
-- the recording a different run.
worldDigest :: IO String
worldDigest = do
  raw <- try (B.readFile defaultWorldFile)
  pure $ case raw :: Either SomeException B.ByteString of
    Left _ -> ""
    Right bytes -> digestOf (map (toEnum . fromIntegral) (B.unpack bytes))

-- | Write a finished run, and say where it went. Nothing if it is not over.
saveReplay :: FilePath -> String -> Int -> String -> GameState -> IO (Maybe FilePath)
saveReplay dir digest seed when state =
  case replayRun record of
    Nothing -> pure Nothing
    Just _ -> do
      let path = dir </> map safe when <.> "json"
      written <- try $ do
        createDirectoryIfMissing True dir
        B.writeFile path (encode record)
      pure $ case written :: Either SomeException () of
        -- A replay that cannot be written must not take the run down with
        -- it; the player is told, and the score still stands.
        Left _ -> Nothing
        Right _ -> Just path
  where
    record = recordOf digest seed state
    safe c = if c `elem` (" :" :: String) then '-' else c

loadReplay :: FilePath -> IO (Either String Replay)
loadReplay path = do
  parsed <- try (eitherDecodeFileStrict path)
  pure $ case parsed :: Either SomeException (Either String Replay) of
    Left err -> Left (path ++ ": " ++ show err)
    Right (Left err) -> Left (path ++ ": " ++ err)
    Right (Right r) -> Right r
