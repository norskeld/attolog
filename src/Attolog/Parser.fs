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

/// Combines two parsers in sequence.
let andThen p1 p2 =
  let parser input =
    let res1 = run p1 input

    match res1 with
    | Success(val1, remaining) ->
      let res2 = run p2 remaining

      match res2 with
      | Success(val2, remaining) ->
        let newValue = (val1, val2)
        Success(newValue, remaining)
      | Failure message -> Failure message
    | Failure message -> Failure message

  Parser parser

/// Infix version of `andThen`.
let (.>>.) = andThen

/// Chooses between two parsers.
let orElse p1 p2 =
  let parser input =
    let res1 = run p1 input

    match res1 with
    | Success _ -> res1
    | Failure _ ->
      let res2 = run p2 input
      res2

  Parser parser

/// Infix version of `orElse`.
let (<|>) = orElse

/// Chooses any of a list of parsers.
let choice parsers = List.reduce orElse parsers

/// Chooses any of a list of characters.
let anyOf chars = chars |> List.map pchar |> choice
