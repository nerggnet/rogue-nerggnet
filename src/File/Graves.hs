-- src/File/Graves.hs
--
-- Where earlier runs of this dungeon ended, kept between games.
module File.Graves
  ( loadGraves
  , recordGrave
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecodeFileStrict, encode)
import Data.Either (fromRight)
import Game.State (treasureCarried)
import Game.Types
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import qualified Data.ByteString.Lazy as B

-- | How many of the dead are kept. Enough that a bad week is visible in the
-- dungeon, few enough that the floors do not fill up with bodies.
keptGraves :: Int
keptGraves = 8

loadGraves :: FilePath -> IO (Either String [Grave])
loadGraves path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right [])
    else do
      parsed <- eitherDecodeFileStrict path
      pure $ case parsed of
        Left err -> Left (path ++ ": " ++ err)
        Right dead -> Right dead

-- | Write down a run that ended badly. Nothing for one that got out: a
-- player who walked home leaves no body to find.
recordGrave :: FilePath -> String -> String -> GameState -> IO (Maybe Grave)
recordGrave path worldDigest when state
  | not (gameOver state) || gameWon state = pure Nothing
  | otherwise = do
      existing <- fromRight [] <$> loadGraves path
      let grave = Grave
            { graveWhen = when
            , graveWorld = worldDigest
            , graveFloor = currentLevel state
            , graveAt = position (player state)
            , graveCarried = inventory (player state)
            , graveTreasure = treasureCarried state
            }
          kept = take keptGraves (grave : existing)
      -- A dungeon of its own keeps its files in a corner of its own, which
      -- may not exist yet. Without this the scoreboard for a pack was
      -- written nowhere, quietly, because the write is guarded.
      written <- try $ do
        createDirectoryIfMissing True (takeDirectory path)
        B.writeFile path (encode kept)
      pure $ case written :: Either SomeException () of
        -- A grave that cannot be written is not worth ending the run over.
        Left _ -> Nothing
        Right _ -> Just grave
