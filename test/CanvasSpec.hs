{-# LANGUAGE OverloadedStrings #-}

module CanvasSpec where

import Commands
import Canvas
import Test.Hspec

spec :: Spec
spec = describe "Canvas" $ do
  it "can render an empty canvas" $ do
    let state = CanvasState []
    render state `shouldBe` ""

  it "can render a vertical line" $ do
    let state = CanvasState [ CreateCanvas 20 5, DrawLine (2, 2) (2, 5) ]
    render state `shouldBe` "----------------------\n\
                            \|                    |\n\
                            \| X                  |\n\
                            \| X                  |\n\
                            \| X                  |\n\
                            \| X                  |\n\
                            \----------------------"

  it "can render a horizonal line" $ do
    let state = CanvasState [ CreateCanvas 20 5, DrawLine (2, 2) (5, 2) ]
    render state `shouldBe` "----------------------\n\
                            \|                    |\n\
                            \| XXXX               |\n\
                            \|                    |\n\
                            \|                    |\n\
                            \|                    |\n\
                            \----------------------"

  it "can render a diagonal line" $ do
    let state = CanvasState [ CreateCanvas 20 5, DrawLine (2, 2) (20, 5) ]
    render state `shouldBe` "----------------------\n\
                            \|                    |\n\
                            \| XXXXXX             |\n\
                            \|       XXXXXX       |\n\
                            \|             XXXXXX |\n\
                            \|                   X|\n\
                            \----------------------"

  it "can render a rectangle" $ do
    let state = CanvasState [ CreateCanvas 20 5, DrawRectangle (2, 2) (5, 5) ]
    render state `shouldBe` "----------------------\n\
                            \|                    |\n\
                            \| XXXX               |\n\
                            \| X  X               |\n\
                            \| X  X               |\n\
                            \| XXXX               |\n\
                            \----------------------"
