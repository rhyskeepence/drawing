# Drawing Exercise (in Haskell)

Run `stack test` to execute all tests

Run `stack exec drawing` to run the program.

# WTF?

Start with [Main.hs](main/Main.hs). 

After outputting some brief usage, it calls `run` in the [Drawing.hs](app/Drawing.hs) module, with an initial state of [] - the empty list.
(this call is wrapped in `runInputT defaultSettings` which is some housekeeping to do with `haskeline`, to support history, etc.)

Run then prompts for user input, and then `parseAndUpdate` with the user input and the current state. `parseAndUpdate` first parses the input by calling `parse` in [Parser.hs](app/Parser.hs), and then `updateState` in [Command.hs](app/Commands.hs). Both of these return `Either`, and they may fail with a `DrawingError`.

The result of `parseAndUpdate` is pattern matched. Errors in parsing or updating the state (ie, commands received out of order) are printed to the screen, and then `run` is called again recursively. A Quit command will return, which has the effect of exiting the program. 

A valid drawing command results in a call to `render` in [Canvas.hs](app/Canvas.hs) with the new state. Then `run` is called recursively to accept the next command. 
  
Rendering a canvas just folds over all the drawing commands received, updating a `Canvas`. The Canvas itself is implemented as an IntMap, with the key being generated using `makeKey`. This turned out to be easier to manage than a 2D array. The line drawing algorithm is [naive](https://en.wikipedia.org/wiki/Line_drawing_algorithm).

The Parser is implemented using `parsec`.
