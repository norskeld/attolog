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
  let tFalse = tag "false"
  let tLeftParen = tag "("
  let tRightParen = tag ")"
  let tComma = tag ","
  let tPeriod = tag "."

  let pName = many (lowercase <|> uppercase <|> digit <|> char '_')
  let pConstant = lowercase .>>. pName |>> collect
  let pVariable = uppercase .>>. pName |>> collect

// ---------------------------------------------------------------------------------------------------------------------
// Forwarded refs for defining recursive parsers:

let private pDocument, private pDocumentRef =
  createForwardedParser<list<Syntax.Command>> ()

let private pExpr, private pExprRef = createForwardedParser<Syntax.Command> ()
let private pQuery, private pQueryRef = createForwardedParser<Syntax.Command> ()
let private pAssert, private pAssertRef = createForwardedParser<Syntax.Command> ()
let private pComment, private pCommentRef = createForwardedParser<Syntax.Command> ()
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
  let pCommand = spaced pExpr .>>. pDocument |>> (fun (expr, file) -> expr :: file)

  return! pEof <|> pCommand
}

// expr:
//   | query   { $1 }
//   | assert  { $1 }
//   | comment { $1 }
define pExprRef { return! pComment <|> pQuery <|> pAssert <?> "expression" }

// query:
//   | QUERY clause PERIOD { Query $2 }
define pQueryRef {
  return!
    Lexical.tQuery >>. pClause .>> Lexical.tPeriod
    |>> (fun (clause) -> Syntax.Query(clause))
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

  return! pAssertionAtom <|> pAssertionClause
}

// comment:
//   | COMMENT { Comment $1 }
define pCommentRef {
  let pCommentStart = tag "--"
  let pCommentEnd = char '\n' <?> "newline"
  let pAny = satisfy ((<>) '\n') "anything but newline"

  return!
    between pCommentStart (many1 pAny) pCommentEnd
    |>> (fun chars -> Syntax.Comment((charsToString chars).Trim()))
    <?> "comment"
}

// atom:
//   | CONST                    { ($1, []) }
//   | CONST LPAREN args RPAREN { ($1, $3) }
define pAtomRef {
  let pAtomArgless = Lexical.pConstant |>> (fun constant -> (constant, []))

  let pAtomArgful =
    Lexical.pConstant .>> Lexical.tLeftParen .>>. pArgs .>> Lexical.tRightParen
    |>> (fun (constant, args) -> (constant, args))

  return! pAtomArgful <|> pAtomArgless
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

  return! pAtomList <|> pAtomSingle <|> pTrue
}

// args:
//   | literal            { [$1] }
//   | literal COMMA args { $1 :: $3 }
define pArgsRef {
  let pArgument = spaced pLiteral |>> (fun literal -> [ literal ])

  let pArguments =
    spaced pLiteral .>> Lexical.tComma .>>. pArgs
    |>> (fun (literal, args) -> literal :: args)

  return! pArguments <|> pArgument
}

// literal:
//   | CONST                    { Const $1 }
//   | VAR                      { Var ($1, 0) }
//   | CONST LPAREN args RPAREN { App ($1, $3) }
define pLiteralRef {
  let pConst = Lexical.pConstant |>> Syntax.Const

  let pVar = Lexical.pVariable |>> (fun var -> Syntax.Var(var, 0))

  let pApp =
    Lexical.pConstant .>>. between Lexical.tLeftParen pArgs Lexical.tRightParen
    |>> (fun (constant, args) -> Syntax.App(constant, args))

  return! pApp <|> pConst <|> pVar
}

// ---------------------------------------------------------------------------------------------------------------------
// Helpers:

/// Runs a given parser `p` on input `input` and prints the result to stdout.
let runAndPrint (p: Parser<_>) (input: string) : unit =
  let result = run p input
  let output = ParserResult<_>.toString result

  printfn "%A" output

/// Parses a given input `input` into a syntax tree.
let parse (input: string) : Result<list<Syntax.Command>, string> =
  match run pDocument input with
  | Success(result, _) -> Ok result
  | Failure(_) as result ->
    let output = ParserResult<_>.toString result
    let message = sprintf "%A" output

    Error message
