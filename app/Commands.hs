module Commands where

import           Data.List (any)
import           Data.Text (Text)

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

updateState :: UserCommand -> CanvasState -> Either DrawingError CanvasState
updateState userCommand state =
  case userCommand of
    CanvasCommand canvasCommand -> validate canvasCommand state
    _                           -> Right state

validate :: CanvasCommand -> CanvasState -> Either DrawingError CanvasState
validate canvasCommand (CanvasState currentCommands) =
  let isCreate (CreateCanvas _ _) = True
      isCreate _                  = False

      hasCreateCanvas = any isCreate currentCommands

      addCommandIfCanvasExists =
        if hasCreateCanvas
          then Right $ CanvasState $ currentCommands ++ [canvasCommand]
          else Left NoCanvas

  in case canvasCommand of
       CreateCanvas _ _   -> Right $ CanvasState $ currentCommands ++ [canvasCommand]
       DrawLine _ _       -> addCommandIfCanvasExists
       DrawRectangle _ _  -> addCommandIfCanvasExists
