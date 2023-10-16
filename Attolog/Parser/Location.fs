[<AutoOpen>]
module Attolog.Parser.Location

/// Represents a location in the input stream.
type Location = {
  line: int
  col: int
} with

  /// Default location.
  static member initial = {
    line = 0
    col = 0
  }

  /// Increments the column number and returns a new `Location`.
  static member incrementCol loc = { loc with col = loc.col + 1 }

  /// Increments the line number and returns a new `Location`.
  static member incrementLine loc = {
    line = loc.line + 1
    col = 0
  }
