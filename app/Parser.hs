{-# LANGUAGE OverloadedStrings #-}

module Parser (parse) where

import Commands
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Text.ParserCombinators.Parsec.Char
import Data.Text
import Data.Either.Combinators
import Control.Monad (void)

parse :: Text -> Either DrawingError UserCommand
parse input =
  let parseResult = P.parse parseCommand "" input
  in mapLeft (ParseError . pack . show) parseResult

parseCommand :: Parser UserCommand
parseCommand = P.choice [parseCreateCanvas, parseDrawLine,parseDrawRectangle, parseQuit]

parseCreateCanvas :: Parser UserCommand
parseCreateCanvas = do
  void $ string "C"
  width <- space >> decimal
  height <- space >> decimal
  return $ CanvasCommand $ CreateCanvas width height

parseDrawLine :: Parser UserCommand
parseDrawLine = do
  void $ string "L"
  x1 <- space >> decimal
  y1 <- space >> decimal
  x2 <- space >> decimal
  y2 <- space >> decimal
  return $ CanvasCommand $ DrawLine (x1, y1) (x2, y2)

parseDrawRectangle :: Parser UserCommand
parseDrawRectangle = do
  void $ string "R"
  x1 <- space >> decimal
  y1 <- space >> decimal
  x2 <- space >> decimal
  y2 <- space >> decimal
  return $ CanvasCommand $ DrawRectangle (x1, y1) (x2, y2)

parseQuit :: Parser UserCommand
parseQuit = do
  void $ string "Q"
  return Quit

decimal = read <$> P.many1 digit

