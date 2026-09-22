-- app/Main.hs
module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import UI.MainUI (startGame, verifyReplay, watchReplay)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> startGame
    ["--replay", path] -> verifyReplay path
    ["--watch", path] -> watchReplay path
    _ -> do
      name <- getProgName
      putStrLn ("usage: " ++ name ++ " [--replay <file> | --watch <file>]")
      exitFailure
