{-# LANGUAGE DeriveGeneric #-}
-- src/Game/Replay.hs
--
-- A run, written down.
--
-- The game is deterministic: the same seed and the same keys against the
-- same dungeon give the same run, every time. So a run does not need to be
-- recorded move by move -- the seed and the keystrokes are the run, and
-- everything else can be worked out again by playing them.
--
-- That is what makes a score worth comparing. The scoreboard says somebody
-- carried 13,100 out of floor 12; a replay is the proof, and can be watched.
-- It is also a test no unit test can write: play a recorded winning run
-- against the dungeon as it stands now and see whether it still wins.
--
-- Nothing here does any IO. Reading and writing the file is File.Replays.
module Game.Replay
  ( Replay (..)
  , digestOf
  , recordOf
  , replay
  , replayStart
  , Divergence (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (shiftL, xor, (.&.))
import Data.Char (intToDigit)

import GHC.Generics (Generic)
import Game.Logic (applyKey)
import Game.Score (runOf)
import Game.State (newGame)
import Game.Types
import Numeric (showHex)
import System.Random (mkStdGen)
import qualified File.Types as FT

-- | A run: which dungeon, which seed, which keys, and what it came to.
data Replay = Replay
  { replayWorld :: String   -- ^ A digest of the world file it was played on
  , replaySeed  :: Int
  , replayKeys  :: String   -- ^ In the order they were pressed
  , replayRun   :: Maybe Run -- ^ What it ended as, to be checked against
  }
  deriving (Eq, Show, Generic)

instance ToJSON Replay
instance FromJSON Replay

-- | A fingerprint of the dungeon a run was played on.
--
-- Editing world.json invalidates a replay just as surely as it invalidates
-- a save: the keys would still be pressed but they would be pressed at
-- different things. FNV-1a over the bytes, which is short, stable across
-- machines, and wants no dependency of its own -- it is a fingerprint and
-- not a signature, and is not asked to resist anybody.
digestOf :: String -> String
digestOf = pad . flip showHex "" . foldl' step 14695981039346656037
  where
    step h c = ((h `xor` fromIntegral (fromEnum c)) * 1099511628211)
                 .&. (1 `shiftL` 64 - 1 :: Integer)
    pad t = replicate (16 - length t) (intToDigit 0) ++ t

-- | The record of a game that has been played to its end.
recordOf :: String -> Int -> GameState -> Replay
recordOf worldDigest seed state = Replay
  { replayWorld = worldDigest
  , replaySeed  = seed
  , replayKeys  = reverse (keysPressed state)
  , replayRun   = runOf "replay" state
  }

-- | Why a replay did not come out as it was written down.
data Divergence
  = WrongDungeon String String -- ^ Expected digest, and the one it was given
  | WouldNotStart [String]     -- ^ The world file would not build a game
  | EndedDifferently Run Run   -- ^ What was recorded, and what happened
  deriving (Eq, Show)

-- | Play the keys back and say where it got to.
--
-- The digest is checked first, because a replay against a dungeon that has
-- been edited since is not a failed run, it is a question that cannot be
-- asked.
replay :: String -> FT.GameConfig -> Replay -> Either Divergence GameState
replay worldDigest config rec = do
  start <- replayStart worldDigest config rec
  let ended = foldl (flip applyKey) start (replayKeys rec)
  case (replayRun rec, runOf "replay" ended) of
    (Just written, Just got) | written /= got -> Left (EndedDifferently written got)
    _ -> Right ended

-- | The game as it stood before the first key, for a watcher to step
-- through. The same checks as replay, minus the playing.
replayStart :: String -> FT.GameConfig -> Replay -> Either Divergence GameState
replayStart worldDigest config rec
  | replayWorld rec /= worldDigest =
      Left (WrongDungeon (replayWorld rec) worldDigest)
  | otherwise = case newGame (mkStdGen (replaySeed rec)) config of
      Left problems -> Left (WouldNotStart problems)
      Right start -> Right start
