{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Commands
import Parser
import Data.Either
import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
  it "can parse C to create canvas" $
    parse "C 20 5" `shouldBe` Right (CanvasCommand(CreateCanvas 20 5))
  it "can parse L to draw a line" $
    parse "L 5 5 15 15" `shouldBe` Right (CanvasCommand(DrawLine (5, 5) (15, 15)))
  it "can parse R to draw a rectangle" $
    parse "R 1 1 20 20" `shouldBe` Right (CanvasCommand(DrawRectangle (1, 1) (20, 20)))
  it "can parse Q for quit" $
    parse "Q" `shouldBe` Right Quit

  it "returns error on incorrect C arguments" $
    parse "C FOO" `shouldSatisfy` isLeft
  it "returns error on incorrect L arguments" $
    parse "L FOO" `shouldSatisfy` isLeft
  it "returns error on incorrect R arguments" $
    parse "R FOO" `shouldSatisfy` isLeft
  it "returns error on unknown command" $
    parse "FOO" `shouldSatisfy` isLeft
