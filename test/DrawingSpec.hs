{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DrawingSpec where

import Drawing
import Commands
import Test.Hspec
import Data.Text (Text)
import Control.Monad.State

spec :: Spec
spec = describe "Drawing Program" $ do
  it "should quit" $
    runWithUserInput ["Q"] `shouldBe` ["Enter Command: "]

  it "should create canvas and draw a line interactively" $
    runWithUserInput ["C 10 5", "L 1 1 2 1", "Q"] `shouldBe` [
        "Enter Command: "
      , "------------\n\
        \|          |\n\
        \|          |\n\
        \|          |\n\
        \|          |\n\
        \|          |\n\
        \------------"
      , "Enter Command: "
      , "------------\n\
        \|XX        |\n\
        \|          |\n\
        \|          |\n\
        \|          |\n\
        \|          |\n\
        \------------"
      , "Enter Command: "]


data InputOutput = InputOutput
  { userInput :: [Text]
  , programOutput :: [Text]
  }

instance UserInput (State InputOutput) where
  prompt message = do
    (InputOutput input output) <- get
    put (InputOutput (tail input) (output ++ [message]))
    return $ head input

  writeLine text = do
    (InputOutput input output) <- get
    put (InputOutput input (output ++ [text]))

runWithUserInput :: [Text] -> [Text]
runWithUserInput input =
  output
  where (InputOutput _ output) = execState (run $ Commands.CanvasState []) (InputOutput input [])
