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

/// Matches a specified character `ch`.
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

/// Maps a `parser`'s result with `f`.
let mapP f parser =
  let parser input =
    let res = run parser input

    match res with
    | Success(value, remaining) ->
      let newValue = f value
      Success(newValue, remaining)
    | Failure message -> Failure message

  Parser parser

/// Infix version of `map`.
let (<!>) = mapP

/// Infix version of `map` to be used in pipelines.
let (|>>) x f = mapP f x

/// Lifts a value.
let returnP x =
  let parser input = Success(x, input)
  Parser parser

/// Lifts a function.
let applyP fP xP =
  (fP .>>. xP) |> mapP (fun (f, x) -> f x)

/// Infix version of `applyP`.
let (<*>) = applyP

/// Lifts a binary function.
let lift2 f xP yP = returnP f <*> xP <*> yP

/// Applies parsers in sequence.
let rec sequence parsers =
  let cons head tail = head :: tail
  let consP = lift2 cons

  match parsers with
  | [] -> returnP []
  | head :: tail -> consP head (sequence tail)

/// Matches a specified string.
let pstring str =
  str
  |> List.ofSeq
  |> List.map pchar
  |> sequence
  |> mapP (fun chars -> chars |> List.toArray |> String)

/// Helper that applies a parser `p` zero or more times collecting values in case of success.
let rec zeroOrMore p input =
  let result = run p input

  match result with
  | Success(firstValue, remainingInput) ->
    let (subsequentValues, remainingInput') = zeroOrMore p remainingInput
    let values = firstValue :: subsequentValues
    (values, remainingInput')
  | Failure _ -> ([], input)

/// Applies a parser `p` zero or more times.
let many p =
  let parser input = Success(zeroOrMore p input)
  Parser parser

/// Applies a parser `p` one or more times.
let many1 p =
  let parser input =
    let result = run p input

    match result with
    | Success(value, remainingInput) ->
      // If first matched, look for zeroOrMore.
      let (subsequentValues, remainingInput') = zeroOrMore p remainingInput
      let values = value :: subsequentValues
      Success(values, remainingInput')
    | Failure message -> Failure message

  Parser parser

/// Matches a parser `p` zero or one time.
let optional p =
  let some = p |>> Some
  let none = returnP None

  some <|> none

/// Parses an integer.
let pint =
  let resultIntoInt (sign, digits) =
    let integer = digits |> List.toArray |> String |> int

    match sign with
    | Some _ -> -integer
    | None -> integer

  let digit = anyOf [ '0' .. '9' ]
  let digits = many1 digit

  optional (pchar '-') .>>. digits |>> resultIntoInt
