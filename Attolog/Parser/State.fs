[<AutoOpen>]
module Attolog.Parser.State

open System

open Attolog.Parser.Location

/// Represents the input stream and its current location.
type State = {
  lines: array<string>
  location: Location
} with

  /// Creates a new `State` from a string.
  static member fromString str =
    if String.IsNullOrEmpty(str) then
      {
        lines = [||]
        location = Location.initial
      }
    else
      let separators = [| "\r\n"; "\n" |]
      let lines = str.Split(separators, StringSplitOptions.None)

      {
        lines = lines
        location = Location.initial
      }

  /// Gets the current line.
  static member currentLine state =
    let line = state.location.line

    if line < state.lines.Length then
      state.lines[line]
    else
      "End of input"

  /// Gets the next character from the input (if any). Returns a tuple of new `State` and the next character.
  static member next state =
    let line = state.location.line
    let col = state.location.col

    if line >= state.lines.Length then
      (state, None)
    else
      let currentLine = State.currentLine state

      if col < currentLine.Length then
        let char = currentLine.[col]
        let nextCol = Location.incrementCol state.location
        let nextState = { state with location = nextCol }
        (nextState, Some char)
      else
        let char = '\n'
        let nextLine = Location.incrementLine state.location
        let nextState = { state with location = nextLine }
        (nextState, Some char)

  /// Checks if given state has reached the end of input.
  static member isEOF state =
    let currentLine = State.currentLine state

    let isColOverflow = state.location.col >= currentLine.Length
    let isLineOverflow = state.location.line >= state.lines.Length

    isColOverflow || isLineOverflow
