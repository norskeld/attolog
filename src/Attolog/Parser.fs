module Attolog.Parser

open System

/// Represents a parsing result.
type Result<'a> =
  | Success of 'a
  | Failure of string

/// Represents a parsing function.
type Parser<'T> = Parser of (string -> Result<'T * string>)

/// Runs a parser against the input stream.
let run p input =
  let (Parser parser) = p
  parser input

/// Takes a parser-producing function `f` and a parser `p` and passes the output of `p` into `f` to create a new parser.
let bindP f p =
  let parser input =
    let result = run p input

    match result with
    | Success(value, remainingInput) ->
      let p2 = f value
      run p2 remainingInput
    | Failure message -> Failure message

  Parser parser

/// Infix version of `bindP`. Flips parameters.
let (>>=) p f = bindP f p

/// Lifts a value into `Parser`.
let returnP x =
  let parser input = Success(x, input)
  Parser parser

/// Lifts a function into `Parser`.
let applyP fP xP =
  fP >>= (fun f -> xP >>= (fun x -> returnP (f x)))

/// Infix version of `applyP`.
let (<*>) = applyP

/// Maps a `parser`'s result with `f`.
let mapP f = bindP (f >> returnP)

/// Infix version of `map`.
let (<!>) = mapP

/// Infix version of `map`. Flips parameters.
let (|>>) x f = mapP f x

/// Lifts a binary function.
let lift2 f xP yP = returnP f <*> xP <*> yP

/// Combines two parsers in sequence.
let andThen p1 p2 =
  p1 >>= (fun res1 -> p2 >>= (fun res2 -> returnP (res1, res2)))

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
let choice ps = List.reduce orElse ps

/// Applies parsers in sequence.
let rec sequence ps =
  let cons head tail = head :: tail
  let consP = lift2 cons

  match ps with
  | [] -> returnP []
  | head :: tail -> consP head (sequence tail)

/// Helper that applies a parser `p` zero or more times collecting values in case of success.
let rec internal zeroOrMore p input =
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
  p >>= (fun head -> many p >>= (fun tail -> returnP (head :: tail)))

/// Matches a parser `p` zero or one time.
let optional p =
  let some = p |>> Some
  let none = returnP None

  some <|> none

/// Keeps only the result of the left side parser.
let (.>>) p1 p2 = p1 .>>. p2 |> mapP (fun (a, _) -> a)

/// Keeps only the result of the right side parser.
let (>>.) p1 p2 = p1 .>>. p2 |> mapP (fun (_, b) -> b)

/// Keeps only the result of the middle parser.
let between left middle right = left >>. middle .>> right

/// Parses one or more occurrences of `p` separated by `sep`.
let sepBy1 p sep =
  let sepThenP = sep >>. p
  p .>>. many sepThenP |>> fun (p, ps) -> p :: ps

/// Parses zero or more occurrences of `p` separated by `sep`.
let sepBy p sep = sepBy1 p sep <|> returnP []

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

/// Chooses any of a list of characters.
let anyOf chars = chars |> List.map pchar |> choice

/// Matches a specified string.
let pstring str =
  str
  |> List.ofSeq
  |> List.map pchar
  |> sequence
  |> mapP (fun chars -> chars |> List.toArray |> String)

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
