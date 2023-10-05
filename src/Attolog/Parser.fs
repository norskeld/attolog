module Attolog.Parser

open System

/// Represents a parsing result.
type Result<'a> =
  | Success of 'a
  | Failure of string

/// Represents a parsing function.
type Parser<'T> = Parser of (string -> Result<'T * string>)

/// Runs a parser against the input stream.
let run parser input =
  let (Parser parser) = parser
  parser input

/// Parses a specified character `ch`.
let pchar ch =
  let parser input =
    if String.IsNullOrEmpty(input) then
      Failure "EOI"
    else
      let first = input.[0]

      if first = ch then
        let remaining = input.[1..]
        Success(ch, remaining)
      else
        let message = sprintf "Expected '%c', but got '%c'" ch first
        Failure message

  Parser parser
