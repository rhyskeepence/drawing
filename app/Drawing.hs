{-# LANGUAGE OverloadedStrings #-}

module Drawing where

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

errorMessage :: DrawingError -> Text
errorMessage e = case e of
  ParseError msg -> msg

run :: UserInput m => CanvasState -> m ()
run state = do
  write "Enter Command: "
  input <- readLine
  let command = parse input
  case command of
    Right Quit ->
      return ()

    Left err -> do
      writeLine $ errorMessage err
      return ()

    Right (CanvasCommand _) ->
      run state
