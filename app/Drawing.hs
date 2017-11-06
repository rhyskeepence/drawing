{-# LANGUAGE OverloadedStrings #-}

module Drawing where

import           Canvas (render)
import           Commands
import           Data.Text
import           Data.Text.IO
import           System.IO
import           Parser

class Monad m => UserInput m where
  readLine :: m Text
  write :: Text -> m ()
  writeLine :: Text -> m ()

instance UserInput IO where
  readLine = Data.Text.IO.getLine
  write text = Data.Text.IO.putStr text >> hFlush stdout
  writeLine = Data.Text.IO.putStrLn

run :: UserInput m => CanvasState -> m ()
run state = do
  write "Enter Command: "
  input <- readLine
  let result = updateState input state
  case result of
    Left (ParseError msg) -> do
      writeLine msg
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



