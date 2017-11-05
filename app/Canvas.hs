{-# LANGUAGE OverloadedStrings #-}

module Canvas (render) where

import Commands
import Control.Monad
import Data.Text (Text, pack, intercalate)
import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L


data Canvas = Canvas
  { canvasWidth :: Int
  , canvasHeight :: Int
  , canvasPixels :: IntMap.IntMap Bool
  }

render :: CanvasState -> Either DrawingError Text
render (CanvasState commands) =
  let
    canvas = foldM applyCommand Nothing commands
  in
    fmap renderCanvasPixels canvas

applyCommand :: Maybe Canvas -> CanvasCommand -> Either DrawingError (Maybe Canvas)
applyCommand canvas command =
  case command of
    CreateCanvas width height ->
      Right $ Just $ Canvas width height IntMap.empty
    DrawLine point1 point2 ->
      drawOnCanvas (drawLine point1 point2) canvas
    DrawRectangle point1 point2 ->
      drawOnCanvas (drawRectangle point1 point2) canvas

drawOnCanvas :: (Canvas -> Canvas) -> Maybe Canvas -> Either DrawingError (Maybe Canvas)
drawOnCanvas f maybeCanvas =
  case maybeCanvas of
    Just canvas -> Right $ Just $ f canvas
    Nothing -> Left NoCanvas

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
    makeRow canvas y = L.foldl (\xAcc x -> xAcc <> pointToText (x, y) canvas) "" [0 .. (canvasWidth canvas)]
  in
    case maybeCanvas of
      Nothing -> ""
      Just canvas ->
        intercalate "\n" $ L.map (makeRow canvas) [0 .. (canvasHeight canvas)]


setPoint :: Point -> Canvas -> Canvas
setPoint point canvas =
  canvas { canvasPixels = IntMap.insert (makeKey canvas point) True (canvasPixels canvas) }

pointToText :: Point -> Canvas -> Text
pointToText point canvas =
  let
    hasPoint = IntMap.findWithDefault False (makeKey canvas point) (canvasPixels canvas)
  in
    if hasPoint then "X" else " "

makeKey :: Canvas -> Point -> Int
makeKey canvas (x, y) = x + (canvasWidth canvas * y)

