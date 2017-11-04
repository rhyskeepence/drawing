module Main where

import Commands
import Drawing

main :: IO ()
main = run $ Commands.CanvasState []
