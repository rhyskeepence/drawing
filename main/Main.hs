module Main where

import Commands
import Drawing
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings $ run $ Commands.CanvasState []
