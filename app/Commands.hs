module Commands where

import           Data.Text

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
  | NoCanvas
  deriving (Eq, Show)

newtype CanvasState = CanvasState
  { commands :: [CanvasCommand]
  }

update :: UserCommand -> CanvasState -> Either DrawingError CanvasState
update userCommand (CanvasState currentCommands) =
  case userCommand of
    CanvasCommand canvasCommand -> Right $ CanvasState $ currentCommands ++ [canvasCommand]
    _                           -> Right $ CanvasState currentCommands
