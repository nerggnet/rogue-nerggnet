-- app/Main.hs
module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import UI.MainUI (startGame, verifyReplay)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> startGame
    ["--replay", path] -> verifyReplay path
    _ -> do
      name <- getProgName
      putStrLn ("usage: " ++ name ++ " [--replay <file>]")
      exitFailure
