module Attolog.Parser

open System

/// Parser label used for error reporting and debugging.
type ParserLabel = string

/// Parser error type used for error reporting.
type ParserError = string

/// Represents a parsing result.
type Result<'a> =
  | Success of 'a
  | Failure of ParserLabel * ParserError

  override this.ToString() =
    match this with
    | Success(value) -> sprintf "%A" value
    | Failure(label, message) -> sprintf "Error in %s:\n  %s" label message

/// Represents a parsing function.
type Parser<'T> = {
  parse: (string -> Result<'T * string>)
  label: ParserLabel
}

/// Runs a parser against the input stream.
let run (p: Parser<_>) input = p.parse input

/// Updates the label in the parser.
let setLabel p label =
  let parse input =
    let res = p.parse input

    match res with
    | Success s -> Success s
    | Failure(_, message) -> Failure(label, message)

  { parse = parse; label = label }

/// Infix version of `setLabel`.
let (<?>) = setLabel

/// Gets the label from a parser.
let getLabel p = p.label

/// Takes a parser-producing function `f` and a parser `p` and passes the output of `p` into `f` to create a new parser.
let bindP f p =
  let label = "unknown"

  let parse input =
    let result = run p input

    match result with
    | Success(value, remainingInput) ->
      let p2 = f value
      run p2 remainingInput
    | Failure(label, message) -> Failure(label, message)

  { parse = parse; label = label }

/// Infix version of `bindP`. Flips parameters.
let (>>=) p f = bindP f p

/// Lifts a value into `Parser`.
let returnP x =
  let label = "returnP"
  let parse input = Success(x, input)

  { parse = parse; label = label }

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
  let label = "orElse" // TODO: Provide a better label inferred from p1 and p2.

  let parse input =
    let res1 = run p1 input

    match res1 with
    | Success _ -> res1
    | Failure _ ->
      let res2 = run p2 input
      res2

  { parse = parse; label = label }

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
  let label = "many" // TODO: Provide a better label inferred from p.
  let parse input = Success(zeroOrMore p input)

  { parse = parse; label = label }

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
  let label = "pchar"

  let parse input =
    if String.IsNullOrEmpty(input) then
      Failure(label, "EOI")
    else
      let first = input.[0]

      if first = ch then
        let remaining = input.[1..]
        Success(ch, remaining)
      else
        let message = sprintf "Expected '%c', but got '%c'" ch first
        Failure(label, message)

  { parse = parse; label = label }

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
