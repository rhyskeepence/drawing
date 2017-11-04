module Commands where

import Data.Text

type Width = Int
type Height = Int
type Point = (Int, Int)

data UserCommand
  = CanvasCommand CanvasCommand
  | Quit
  deriving (Eq, Show)

data CanvasCommand
  = CreateCanvas Width Height
  | DrawLine Point Point
  | DrawRectangle Point Point
  deriving (Eq, Show)

data DrawingError
  = ParseError Text
  deriving (Eq, Show)

newtype CanvasState = CanvasState
  { commands :: [CanvasCommand]
  }

update :: CanvasCommand -> CanvasState -> CanvasState
update command currentState =
  case command of
    CreateCanvas width height ->
      currentState
    DrawLine from to ->
      currentState
    DrawRectangle from to ->
      currentState


