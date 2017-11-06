{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DrawingSpec where

import Drawing
import Commands
import Test.Hspec
import Data.Text (Text)
import Control.Monad.State

data InputOutput = InputOutput
  { userInput :: [Text]
  , programOutput :: [Text]
  }

instance UserInput (State InputOutput) where
  readLine = do
    (InputOutput input output) <- get
    put (InputOutput (tail input) output)
    return $ head input

  write text = do
    (InputOutput input output) <- get
    put (InputOutput input (output ++ [text]))

  writeLine text = do
    (InputOutput input output) <- get
    put (InputOutput input (output ++ [text]))


spec :: Spec
spec = describe "Drawing Program" $ do
  it "should quit" $
    runWithUserInput ["Q"] `shouldBe` ["Enter Command: "]

  it "should create canvas" $
    runWithUserInput ["C 10 5", "Q"] `shouldBe` [
        "Enter Command: "
      , "           \n\
        \           \n\
        \           \n\
        \           \n\
        \           \n\
        \           "
      , "Enter Command: "]


runWithUserInput :: [Text] -> [Text]
runWithUserInput input =
  output
  where (InputOutput _ output) = execState (run $ Commands.CanvasState []) (InputOutput input [])
