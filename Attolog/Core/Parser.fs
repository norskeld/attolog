module Attolog.Core.Parser

open Attolog.Parser

// ---------------------------------------------------------------------------------------------------------------------
// Lexical tokens (terminals) and some non-terminal stuff:

module private Lexical =
  /// Collects a list of chars into a string.
  let collect (first, rest) = charsToString (first :: rest)

  let tAssert = tag ":-"
  let tQuery = tag "?-"
  let tTrue = tag "true"
  let tLeftParen = tag "("
  let tRightParen = tag ")"
  let tComma = tag ","
  let tPeriod = tag "."

  let pNameBase = lowercase <|> uppercase <|> digit <|> char '_'

  let pConstant =
    lowercase .>>. many (pNameBase <|> char '-') |>> collect <?> "constant"

  let pVariable = uppercase .>>. many pNameBase |>> collect <?> "variable"

// ---------------------------------------------------------------------------------------------------------------------
// Forwarded refs for defining recursive parsers:

let private pDocument, private pDocumentRef =
  createForwardedParser<list<Syntax.Command>> ()

let private pExpr, private pExprRef = createForwardedParser<Syntax.Command> ()
let private pQuery, private pQueryRef = createForwardedParser<Syntax.Command> ()
let private pAssert, private pAssertRef = createForwardedParser<Syntax.Command> ()
let private pAtom, private pAtomRef = createForwardedParser<Syntax.Atom> ()
let private pClause, private pClauseRef = createForwardedParser<Syntax.Clause> ()
let private pArgs, private pArgsRef = createForwardedParser<list<Syntax.Term>> ()
let private pLiteral, private pLiteralRef = createForwardedParser<Syntax.Term> ()

// ---------------------------------------------------------------------------------------------------------------------
// Definitions:

// file:
//   | expr file { $1 :: $2 }
//   | EOF       { [] }
define pDocumentRef {
  let pEof = (eof >>% [])

  let pCommand =
    spaced pExpr .>>. pDocument |>> (fun (expr, file) -> expr :: file) <?> "command"

  return! pEof <|> pCommand
}

// expr:
//   | query   { $1 }
//   | assert  { $1 }
define pExprRef { return! pQuery <|> pAssert <?> "expression" }

// query:
//   | QUERY clause PERIOD { Query $2 }
define pQueryRef {
  return!
    spaced Lexical.tQuery >>. pClause .>> spaced Lexical.tPeriod
    |>> (fun (clause) -> Syntax.Query(clause))
    <?> "query"
}

// assert:
//   | atom PERIOD               { Assert ($1, []) }
//   | atom ASSERT clause PERIOD { Assert ($1, $3) }
define pAssertRef {
  let pAssertionAtom =
    pAtom .>> Lexical.tPeriod |>> (fun atom -> Syntax.Assert(atom, []))

  let pAssertionClause =
    pAtom .>> spaced Lexical.tAssert .>>. pClause .>> spaced Lexical.tPeriod
    |>> (fun (atom, clause) -> Syntax.Assert(atom, clause))

  return! pAssertionAtom <|> pAssertionClause <?> "assertion"
}

// atom:
//   | CONST                    { ($1, []) }
//   | CONST LPAREN args RPAREN { ($1, $3) }
define pAtomRef {
  let pAtomArgless = Lexical.pConstant |>> (fun constant -> (constant, []))

  let pAtomArgful =
    Lexical.pConstant .>> Lexical.tLeftParen .>>. pArgs .>> Lexical.tRightParen
    |>> (fun (constant, args) -> (constant, args))

  return! pAtomArgful <|> pAtomArgless <?> "atom"
}

// clause:
//   | TRUE              { [] }
//   | atom              { [$1] }
//   | atom COMMA clause { $1 :: $3 }
define pClauseRef {
  let pTrue = spaced Lexical.tTrue >>% []

  let pAtomSingle = pAtom |>> (fun atom -> [ atom ])

  let pAtomList =
    spaced pAtom .>> Lexical.tComma .>>. spaced pClause
    |>> (fun (atom, clause) -> atom :: clause)

  return! pTrue <|> pAtomList <|> pAtomSingle <?> "clause"
}

// args:
//   | literal            { [$1] }
//   | literal COMMA args { $1 :: $3 }
define pArgsRef {
  let pArgument = spaced pLiteral |>> (fun literal -> [ literal ])

  let pArguments =
    spaced pLiteral .>> Lexical.tComma .>>. pArgs
    |>> (fun (literal, args) -> literal :: args)

  return! pArguments <|> pArgument <?> "args"
}

// literal:
//   | CONST                    { Const $1 }
//   | VAR                      { Var ($1, 0) }
//   | CONST LPAREN args RPAREN { App ($1, $3) }
define pLiteralRef {
  let pConst = Lexical.pConstant |>> Syntax.Const <?> "constant"

  let pVar = Lexical.pVariable |>> (fun var -> Syntax.Var(var, 0)) <?> "variable"

  let pApp =
    Lexical.pConstant .>>. between Lexical.tLeftParen pArgs Lexical.tRightParen
    |>> (fun (constant, args) -> Syntax.App(constant, args))
    <?> "application"

  return! pApp <|> pConst <|> pVar
}

// ---------------------------------------------------------------------------------------------------------------------
// Helpers:

/// State config.
let config = { comment = Some "%" }

/// Runs the root parser on input `input` and prints the result to stdout.
let runAndPrint (input: string) : unit =
  let result = runWithConfig pDocument config input
  let output = ParserResult<_>.toString result

  printfn "%O" output

/// Parses a given input `input` into a syntax tree.
let parse (input: string) : Result<list<Syntax.Command>, string> =
  match runWithConfig pDocument config input with
  | Success(result, _) -> Ok result
  | Failure(_) as result ->
    let output = ParserResult<_>.toString result
    let message = sprintf "%O" output

    Error message
