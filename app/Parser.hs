{-# LANGUAGE OverloadedStrings #-}

module Parser where

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
parseCommand = do
  void $ string "Q"
  return Quit

