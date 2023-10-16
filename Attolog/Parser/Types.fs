[<AutoOpen>]
module Attolog.Parser.Types

open Attolog.Parser.State

/// Parser label used for error reporting and debugging.
type ParserLabel = string

/// Parser error type used for error reporting.
type ParserError = string

/// Stores information about the parser location for error messages.
type ParserLocation = {
  line: int
  column: int
  currentLine: string
} with

  /// Converts an `State` into a `ParserLocation`.
  static member fromState state = {
    line = state.location.line
    column = state.location.col
    currentLine = State.currentLine state
  }

/// Represents a parsing result.
type ParserResult<'a> =
  | Success of 'a
  | Failure of ParserLabel * ParserError * ParserLocation

  /// Converts a `ParserResult` into a string.
  static member toString(res: ParserResult<_ * State>) =
    match res with
    | Success(value, _) -> sprintf "%A" value
    | Failure(label, message, location) ->
      let line = location.line + 1
      let column = location.column
      let currentLine = location.currentLine

      let locationPrefix = $"{line} | "
      let locationPrefixOffset = column + locationPrefix.ToString().Length

      let message = sprintf "%*s^ %s" locationPrefixOffset "" message

      sprintf $"Error parsing %s{label}:\n%s{locationPrefix}%s{currentLine}\n%s{message}"

/// Represents a parsing function.
type Parser<'T> = {
  parse: (State -> ParserResult<'T * State>)
  label: ParserLabel
}
