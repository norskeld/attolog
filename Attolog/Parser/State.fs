[<AutoOpen>]
module Attolog.Parser.State

open System

open Attolog.Parser

/// Represents state configuration used to alter the parsing process. Currently used for:
///
/// - Stripping single-line comments using specified comment prefix.
type StateConfig = {
  comment: option<string>
} with

  /// Creates a new `StateConfig`.
  static member initial = { comment = None }

/// Represents the input stream and its current location.
type State = {
  lines: array<string>
  location: Location
} with

  /// Creates a new `State` from a string.
  static member create (str: string) (config: StateConfig) =
    if String.IsNullOrEmpty(str) then
      {
        lines = [||]
        location = Location.initial
      }
    else
      let lines = str.Split([| "\r\n"; "\n" |], StringSplitOptions.None)

      let lines =
        match config.comment with
        | Some comment ->
          let uncommenter (line: string) =
            let index = line.IndexOf(comment)
            if index >= 0 then line.Substring(0, index) else line

          Array.map uncommenter lines
        | None -> lines

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
      "<eof>"

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
