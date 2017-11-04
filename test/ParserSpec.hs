{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Commands
import Parser
import Data.Either
import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
  it "can parse Q for quit" $
    parse "Q" `shouldBe` Right Quit
  it "errors on unknown command" $
    parse "FOO" `shouldSatisfy` isLeft
