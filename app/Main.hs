-- app/Main.hs
module Main where

import File.Paths (defaultWorldFile, pathsFor)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import UI.MainUI (startGame, verifyReplay, watchReplay)

-- | The game is an engine; world.json is the course it ships with.
-- "--world" plays a different one, and everything that run leaves behind --
-- its save, its scoreboard, its dead, its recordings -- goes with it.
main :: IO ()
main = do
  args <- getArgs
  let (world, rest) = case args of
        ("--world" : path : more) -> (path, more)
        _ -> (defaultWorldFile, args)
      paths = pathsFor world
  case rest of
    [] -> startGame paths
    ["--replay", path] -> verifyReplay paths path
    ["--watch", path] -> watchReplay paths path
    _ -> do
      name <- getProgName
      putStrLn ("usage: " ++ name ++ " [--world <dungeon.json>] [--replay <file> | --watch <file>]")
      exitFailure
