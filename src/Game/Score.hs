-- src/Game/Score.hs
--
-- What a finished run was worth, and how one compares with another.
--
-- The dungeon is drawn by hand and never changes, which is the whole reason
-- to keep these: two people playing it are playing the same twelve floors,
-- so their runs can be set side by side. A run that is not written down
-- cannot be.
--
-- Nothing here does any IO or knows about a terminal. Reading and writing
-- the file is File.Scores; drawing the table is UI.Draw.
module Game.Score
  ( runScore
  , runOf
  , ranked
  , placeOf
  ) where

import Data.List (sortOn)
import Game.State (treasureCarried)
import Game.Types

-- | The one number runs are ranked on.
--
-- Treasure is the bulk of it, and dying forfeits all of it, so the decision
-- the dungeon is built around -- press on or turn back -- is the decision
-- the score rewards. Depth is still worth something to a run that ended
-- badly, because getting to floor 11 and dying is not the same as drowning
-- on floor 2.
runScore :: Run -> Int
runScore run = runTreasure run + depthBonus * runDepth run

depthBonus :: Int
depthBonus = 100

-- | The record of a finished game. Nothing while it is still being played.
runOf :: String -> GameState -> Maybe Run
runOf when state
  | not (gameOver state || gameWon state) = Nothing
  | otherwise = Just Run
      { runWhen     = when
      , runEnding   = if gameWon state then GotOut else Killed
      , runDepth    = deepestLevel state + 1
      , runTreasure = if gameWon state then treasureCarried state else 0
      , runXP       = xp (player state)
      , runTurns    = turnCount state
      }

-- | Best first. Ties go to the shorter run, so two identical hauls are
-- separated by who wasted less time getting them.
ranked :: [Run] -> [Run]
ranked = sortOn (\r -> (negate (runScore r), runTurns r))

-- | Where a run comes in a table, counting from 1. Nothing if it is not in it.
placeOf :: Run -> [Run] -> Maybe Int
placeOf run runs =
  case [i | (i, r) <- zip [1 ..] (ranked runs), r == run] of
    (i : _) -> Just i
    _       -> Nothing
