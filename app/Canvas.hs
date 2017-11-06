{-# LANGUAGE OverloadedStrings #-}

module Canvas (render) where

import Commands
import Data.Text (Text, intercalate)
import Data.Monoid
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L

data Canvas = Canvas
  { canvasWidth :: Int
  , canvasHeight :: Int
  , canvasPixels :: IntMap.IntMap Bool
  }

render :: CanvasState -> Text
render (CanvasState canvasCommands) =
  let
    canvas = foldl applyCommand Nothing canvasCommands
  in
    renderCanvasPixels canvas

applyCommand :: Maybe Canvas -> CanvasCommand -> Maybe Canvas
applyCommand canvas command =
  case command of
    CreateCanvas width height ->
      Just $ Canvas width height IntMap.empty
    DrawLine point1 point2 ->
      fmap (drawLine point1 point2) canvas
    DrawRectangle point1 point2 ->
      fmap (drawRectangle point1 point2) canvas

drawLine :: Point -> Point -> Canvas -> Canvas
drawLine (x1, y1) (x2, y2) canvas =
  if dx > dy
    then L.foldl
          (\acc x -> let y = y1 + truncate (dy * fromIntegral (x - x1) / dx) in setPoint (x, y) acc)
          canvas [min x1 x2 .. max x1 x2]
    else L.foldl
          (\acc y -> let x = x1 + truncate (dx * fromIntegral (y - y1) / dy) in setPoint (x, y) acc)
          canvas [min y1 y2 .. max y1 y2]
  where
    dx = fromIntegral (x2 - x1) :: Double
    dy = fromIntegral (y2 - y1) :: Double

drawRectangle :: Point -> Point -> Canvas -> Canvas
drawRectangle (x1, y1) (x2, y2) canvas =
  L.foldl
   (\acc (point1, point2) -> drawLine point1 point2 acc)
   canvas
   [((x1, y1), (x1, y2))
   ,((x1, y1), (x2, y1))
   ,((x1, y2), (x2, y2))
   ,((x2, y1), (x2, y2))]

renderCanvasPixels :: Maybe Canvas -> Text
renderCanvasPixels maybeCanvas =
  let
    makeRow canvas y = L.foldl (\xAcc x -> xAcc <> pointToText (x, y) canvas) "" [0 .. canvasWidth canvas + 1]
  in
    case maybeCanvas of
      Nothing -> ""
      Just canvas ->
        intercalate "\n" $ L.map (makeRow canvas) [0 .. canvasHeight canvas + 1]

setPoint :: Point -> Canvas -> Canvas
setPoint point canvas =
  canvas { canvasPixels = IntMap.insert (makeKey canvas point) True (canvasPixels canvas) }

pointToText :: Point -> Canvas -> Text
pointToText point canvas =
  let
    (x, y) = point
    hasPoint = IntMap.findWithDefault False (makeKey canvas point) (canvasPixels canvas)
    isHorizontalBorder = (y == 0 || y == canvasHeight canvas + 1)
    isVerticalBorder = (x == 0 || x == canvasWidth canvas + 1)
  in
    if isHorizontalBorder then "-"
    else if isVerticalBorder then "|"
    else if hasPoint then "X" else " "

makeKey :: Canvas -> Point -> Int
makeKey canvas (x, y) = x + (canvasWidth canvas * y)
