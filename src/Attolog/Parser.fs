module Attolog.Parser

open System

/// Represents a location in the input stream.
type Location = {
  line: int
  col: int
}

module Location =
  /// Default location.
  let initial = {
    line = 0
    col = 0
  }

  /// Increments the column number and returns a new `Location`.
  let incrementCol loc = { loc with col = loc.col + 1 }

  /// Increments the line number and returns a new `Location`.
  let incrementLine loc = {
    line = loc.line + 1
    col = 0
  }

/// Represents the input stream and its current location.
type InputState = {
  lines: array<string>
  location: Location
}

module InputState =
  /// Creates a new `InputState` from a string.
  let fromString str =
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
  let currentLine state =
    let line = state.location.line

    if line < state.lines.Length then
      state.lines[line]
    else
      "EOI"

  /// Gets the next character from the input (if any). Returns a tuple of new `InputState` and the next character.
  let next state =
    let line = state.location.line
    let col = state.location.col

    if line >= state.lines.Length then
      (state, None)
    else
      let currentLine = currentLine state

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

/// Parser label used for error reporting and debugging.
type ParserLabel = string

/// Parser error type used for error reporting.
type ParserError = string

/// Stores information about the parser location for error messages.
type ParserLocation = {
  line: int
  column: int
  currentLine: string
}

module ParserLocation =
  /// Converts an `InputState` into a `ParserLocation`.
  let fromInputState state = {
    line = state.location.line
    column = state.location.col
    currentLine = InputState.currentLine state
  }

/// Represents a parsing result.
type ParserResult<'a> =
  | Success of 'a
  | Failure of ParserLabel * ParserError * ParserLocation

module ParserResult =
  /// Converts a `ParserResult` into a string.
  let toString (res: ParserResult<_ * InputState>) =
    match res with
    | Success(value, _) -> sprintf "%O" value
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
  parse: (InputState -> ParserResult<'T * InputState>)
  label: ParserLabel
}

/// Runs a parser on a `InputState`.
let runOnInput p input = p.parse input

/// Runs a parser against the input stream.
let run p input =
  runOnInput p (InputState.fromString input)

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

/// Lifts a binary function.
let lift2 f xP yP = returnP f <*> xP <*> yP

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

/// Helper that applies a parser `p` zero or more times collecting values in case of success.
let rec internal zeroOrMore p input =
  let result = runOnInput p input

  match result with
  | Success(firstValue, remainingInput) ->
    let (subsequentValues, remainingInput') = zeroOrMore p remainingInput
    let values = firstValue :: subsequentValues
    (values, remainingInput')
  | Failure _ -> ([], input)

/// Converts a list of chars to a string.
let charsToString chars = chars |> List.toArray |> String

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
  let some = p |>> Some
  let none = returnP None

  some <|> none

/// Keeps only the result of the left side parser.
let (.>>) p1 p2 =
  p1 .>>. p2 |>> (fun (a, _) -> a) <?> getLabel p1

/// Keeps only the result of the right side parser.
let (>>.) p1 p2 =
  p1 .>>. p2 |>> (fun (_, b) -> b) <?> getLabel p2

/// Keeps only the result of the middle parser.
let between left middle right =
  left >>. middle .>> right <?> getLabel middle

/// Parses one or more occurrences of `p` separated by `sep`.
let sepBy1 p sep =
  let sepThenP = sep >>. p
  p .>>. many sepThenP |>> fun (p, ps) -> p :: ps

/// Parses zero or more occurrences of `p` separated by `sep`.
let sepBy p sep = sepBy1 p sep <|> returnP []

/// Helper that allows to build more specialized combinators by providing a predicate for a character.
let satisfy predicate label =
  let parse input =
    let remainingInput, char = InputState.next input

    match char with
    | Some first ->
      if predicate first then
        Success(first, remainingInput)
      else
        let message = sprintf "Unexpected '%c'" first
        let location = ParserLocation.fromInputState input
        Failure(label, message, location)
    | None ->
      let message = "EOI"
      let location = ParserLocation.fromInputState input
      Failure(label, message, location)

  {
    parse = parse
    label = label
  }

/// Matches a specified character `ch`.
let pchar ch =
  let predicate = (=) ch
  let label = sprintf "%c" ch

  satisfy predicate label

/// Chooses any of a list of characters.
let anyOf chars =
  let label = sprintf "any of %A" chars
  chars |> List.map pchar |> choice <?> label

/// Matches a specified string.
let pstring str =
  let label = str

  str |> List.ofSeq |> List.map pchar |> sequence |>> charsToString <?> label

/// Parses a digit.
let pdigit =
  let label = "digit"
  let predicate = Char.IsDigit

  satisfy predicate label

/// Parses a sequence of digits.
let pdigits =
  let label = "digits"
  many1 pdigit |>> charsToString <?> label

/// Parses an integer.
let pint =
  let label = "int"

  let resultIntoInt (sign, digits) =
    let number = digits |> int

    match sign with
    | Some _ -> -number
    | None -> number

  opt (pchar '-') .>>. pdigits |>> resultIntoInt <?> label

/// Parses a float number.
let pfloat =
  let label = "float"

  // helper
  let toFloat (((sign, whole), _), fractions) =
    let number = sprintf "%s.%s" whole fractions |> float

    match sign with
    | Some _ -> -number
    | None -> number

  opt (pchar '-') .>>. pdigits .>>. pchar '.' .>>. pdigits |>> toFloat <?> label

/// Parses a single whitespace character.
let pwhitespace =
  let label = "whitespace"
  let predicate = Char.IsWhiteSpace

  satisfy predicate label

/// Parses zero or more whitespace characters.
let pspaces = many pwhitespace

/// Parses one or more whitespace characters.
let pspaces1 = many1 pwhitespace
