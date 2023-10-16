[<AutoOpen>]
module Attolog.Parser.Library

open System

open Attolog.Prelude
open Attolog.Parser.State
open Attolog.Parser.Types

/// Runs a parser on a `State`.
let runOnInput p input = p.parse input

/// Runs a parser against the input stream.
let run p input = runOnInput p (State.fromString input)

/// Helper that applies a parser `p` zero or more times collecting values in case of success.
let rec internal zeroOrMore p input =
  let result = runOnInput p input

  match result with
  | Success(firstValue, remainingInput) ->
    let (subsequentValues, remainingInput') = zeroOrMore p remainingInput
    let values = firstValue :: subsequentValues
    (values, remainingInput')
  | Failure _ -> ([], input)

/// Helper that converts a list of chars to a string.
let charsToString chars : string = chars |> List.toArray |> String

/// Creates a parser forwarded to ref.
let createForwardedParser<'a> () =
  // Dummy parser.
  let unfixed: Parser<'a> =
    let label = "unfixed"
    let parse _ = failwith "Unfixed forwarded parser"

    {
      parse = parse
      label = label
    }

  let parserRef = ref unfixed

  // Runs the forwarded parser.
  let wrapper =
    let label = "forwarded"
    let parse input = runOnInput !parserRef input

    {
      parse = parse
      label = label
    }

  wrapper, parserRef

/// Updates the label in the parser.
let setLabel p label =
  let parse input =
    let res = p.parse input

    match res with
    | Success s -> Success s
    | Failure(_, message, location) -> Failure(label, message, location)

  {
    parse = parse
    label = label
  }

/// Infix version of `setLabel`.
let (<?>) = setLabel

/// Gets the label from a parser.
let getLabel p = p.label

/// Takes a parser-producing function `f` and a parser `p` and passes the output of `p` into `f` to create a new parser.
let bindP f p =
  let label = "unknown"

  let parse input =
    let result = runOnInput p input

    match result with
    | Success(value, remainingInput) ->
      let p2 = f value
      runOnInput p2 remainingInput
    | Failure(label, message, location) -> Failure(label, message, location)

  {
    parse = parse
    label = label
  }

/// Infix version of `bindP`. Flips parameters.
let (>>=) p f = bindP f p

/// Lifts a value into `Parser`.
let returnP x =
  let label = "returnP"
  let parse input = Success(x, input)

  {
    parse = parse
    label = label
  }

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

/// Lifts a binary function into `Parser`.
let lift2 f xP yP = returnP f <*> xP <*> yP

[<AutoOpen>]
module Define =
  /// Computation expression builder to define forwarded parsers.
  type DefineBuilder<'T>(forwaredRef: ref<Parser<'T>>) =
    member _.Bind(p, f) : Parser<_> = bindP f p

    member _.Return(x) : unit =
      let result = returnP x
      forwaredRef := result

    member _.ReturnFrom(x: Parser<_>) : unit = forwaredRef := x

  /// Computation expression to define forwarded parsers.
  let define forwardedRef = new DefineBuilder<_>(forwardedRef)


/// Combines two parsers in sequence.
let andThen p1 p2 =
  let label = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
  p1 >>= (fun res1 -> p2 >>= (fun res2 -> returnP (res1, res2))) <?> label

/// Infix version of `andThen`.
let (.>>.) = andThen

/// Chooses between two parsers.
let orElse p1 p2 =
  let label = sprintf "%s orElse %s" (getLabel p1) (getLabel p2)

  let parse input =
    let res1 = runOnInput p1 input

    match res1 with
    | Success _ -> res1
    | Failure _ ->
      let res2 = runOnInput p2 input
      res2

  {
    parse = parse
    label = label
  }

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

/// Applies a parser `p` zero or more times.
let many p =
  let label = sprintf "many %s" (getLabel p)
  let parse input = Success(zeroOrMore p input)

  {
    parse = parse
    label = label
  }

/// Applies a parser `p` one or more times.
let many1 p =
  let label = sprintf "many1 %s" (getLabel p)
  p >>= (fun head -> many p >>= (fun tail -> returnP (head :: tail))) <?> label

/// Matches a parser `p` zero or one time.
let opt p =
  let label = sprintf "opt %s" (getLabel p)

  let some = p |>> Some
  let none = returnP None

  some <|> none <?> label

/// Keeps only the result of the left side parser.
let (.>>) p1 p2 =
  p1 .>>. p2 |>> (fun (a, _) -> a) <?> getLabel p1

/// Keeps only the result of the right side parser.
let (>>.) p1 p2 =
  p1 .>>. p2 |>> (fun (_, b) -> b) <?> getLabel p2

/// Applies a parser `p`, ignores the result and returns `x`.
let (>>%) p x = p |>> (fun _ -> x)

/// Keeps only the result of the middle parser.
let between left middle right =
  left >>. middle .>> right <?> getLabel middle

/// Parses one or more occurrences of `p` separated by `sep`.
let sepBy1 p sep =
  let label = sprintf "sepBy1 %s %s" (getLabel p) (getLabel sep)
  let sepThenP = sep >>. p
  p .>>. many sepThenP |>> (fun (p, ps) -> p :: ps) <?> label

/// Parses zero or more occurrences of `p` separated by `sep`.
let sepBy p sep =
  let label = sprintf "sepBy %s %s" (getLabel p) (getLabel sep)
  sepBy1 p sep <|> returnP [] <?> label

/// Helper that allows to build more specialized combinators by providing a predicate for a character.
let satisfy predicate label =
  let parse input =
    let remainingInput, char = State.next input

    match char with
    | Some first ->
      if predicate first then
        Success(first, remainingInput)
      else
        let message = sprintf "Unexpected '%c'" first
        let location = ParserLocation.fromState input
        Failure(label, message, location)
    | None ->
      let message = "Unexpected end of input"
      let location = ParserLocation.fromState input
      Failure(label, message, location)

  {
    parse = parse
    label = label
  }

/// Matches a specified character `ch`.
let char ch =
  let predicate = (=) ch
  let label = sprintf "%c" ch

  satisfy predicate label

/// Matches a character from the given `chars` list.
let anyOf chars =
  let label = sprintf "any of %A" chars
  chars |> List.map char |> choice <?> label

/// Matches a specified string.
let tag str =
  let label = str

  str |> List.ofSeq |> List.map char |> sequence |>> charsToString <?> label

/// Parses a digit.
let digit =
  let label = "digit"
  let predicate = Char.IsDigit

  satisfy predicate label

/// Parses a sequence of digits.
let digits =
  let label = "digits"
  many1 digit |>> charsToString <?> label

/// Parses an integer.
let int =
  let label = "int"

  let resultIntoInt (sign, digits) =
    let number = digits |> int

    match sign with
    | Some _ -> -number
    | None -> number

  opt (char '-') .>>. digits |>> resultIntoInt <?> label

/// Parses a float number.
let float =
  let label = "float"

  // helper
  let toFloat (((sign, whole), _), fractions) =
    let number = sprintf "%s.%s" whole fractions |> float

    match sign with
    | Some _ -> -number
    | None -> number

  opt (char '-') .>>. digits .>>. char '.' .>>. digits |>> toFloat <?> label

/// Parses a single whitespace character.
let whitespace =
  let label = "whitespace"
  let predicate = Char.IsWhiteSpace

  satisfy predicate label

/// Parses zero or more whitespace characters.
let spaces =
  let label = "spaces"
  many whitespace <?> label

/// Parses one or more whitespace characters.
let spaces1 =
  let label = "spaces"
  many1 whitespace <?> label

/// Returns a parser `p` surrounded by whitespace.
let spaced p = between spaces p spaces

/// Parses a lowercase letter.
let lowercase =
  let label = "lowercase letter"
  let predicate = Char.IsLower

  satisfy predicate label

/// Parses an uppercase letter.
let uppercase =
  let label = "uppercase letter"
  let predicate = Char.IsUpper

  satisfy predicate label

/// Matches the end of input.
let eof =
  let label = "eof"

  let parse input =
    let remainingInput, _ = State.next input

    if State.isEOF remainingInput then
      Success((), remainingInput)
    else
      let message = "Expected end of input"
      let location = ParserLocation.fromState input
      Failure(label, message, location)

  {
    parse = parse
    label = label
  }
