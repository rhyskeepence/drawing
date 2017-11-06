{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Drawing where

import           Canvas (render)
import           Commands
import           Data.Text (Text, pack, unpack)
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           System.Console.Haskeline
import           Parser

class Monad m => UserInput m where
  prompt :: Text -> m Text
  writeLine :: Text -> m ()

run :: UserInput m => CanvasState -> m ()
run state = do
  input <- prompt "Enter Command: "
  let result = updateState input state
  case result of
    Left (ParseError msg) -> do
      writeLine $ "\ESC[1;31m" <> msg <> "\ESC[0m\STX"
      run state

    Left NoCanvas -> do
      writeLine "You must first create a canvas."
      run state

    Right (Quit, _) ->
      return ()

    Right (_, newState) -> do
      writeLine $ render newState
      run newState

updateState :: Text -> CanvasState -> Either DrawingError (UserCommand, CanvasState)
updateState input state = do
  command <- parse input
  newState <- update command state
  return (command, newState)


instance UserInput (InputT IO) where
  prompt question = do
    maybeLine <- getInputLine $ unpack question
    return $ pack $ fromMaybe "" maybeLine

  writeLine line = outputStrLn $ unpack line
