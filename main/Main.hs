module Main where

import           Commands
import           Drawing
import           System.Console.Haskeline

main :: IO ()
main = do
  putStrLn "Usage: "
  putStrLn " C <width> <height>       Create a new canvas"
  putStrLn " L <x1> <y1> <x2> <y2>    Draw a line"
  putStrLn " R <x1> <y1> <x2> <y2>    Draw a rectangle"
  putStrLn " Q                        Quit the program"
  runInputT defaultSettings $ run $ Commands.CanvasState []
