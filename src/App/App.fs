open Attolog
open Attolog.Parser

let execute p input =
  let result = run p input
  let output = ParserResult.toString result

  printfn "%A" output

// ---------------------------------------------------------------------------------------------------------------------
// Terminals:

let tAssert = pstring ":-"
let tQuery = pstring "?-"
let tTrue = pstring "true"
let tFalse = pstring "false"
let tLeftParen = pstring "("
let tRightParen = pstring ")"
let tComma = pstring ","
let tPeriod = pstring "."

// ---------------------------------------------------------------------------------------------------------------------
// Non-terminals:

let pConstant =
  plowercase .>>. many (plowercase <|> puppercase <|> pdigit <|> pchar '_')
  |>> (fun (first, rest) -> charsToString (first :: rest))

let pVariable =
  puppercase .>>. many (plowercase <|> puppercase <|> pdigit <|> pchar '_')
  |>> (fun (first, rest) -> charsToString (first :: rest))

// ---------------------------------------------------------------------------------------------------------------------
// Refs:

let pFile, pFileRef = createForwardedParser<list<Syntax.Command>> ()
let pExpr, pExprRef = createForwardedParser<Syntax.Command> ()
let pQuery, pQueryRef = createForwardedParser<Syntax.Command> ()
let pAssert, pAssertRef = createForwardedParser<Syntax.Command> ()
let pAtom, pAtomRef = createForwardedParser<Syntax.Atom> ()
let pClause, pClauseRef = createForwardedParser<Syntax.Clause> ()
let pArgs, pArgsRef = createForwardedParser<list<Syntax.Term>> ()
let pLiteral, pLiteralRef = createForwardedParser<Syntax.Term> ()

// ---------------------------------------------------------------------------------------------------------------------
// Definitions:

// file:
//   | EOF       { [] }
//   | expr file { $1 :: $2 }
define pFileRef {
  let pCommand = spaced pExpr .>>. pFile |>> (fun (expr, file) -> expr :: file)

  return! pCommand <|> returnP []
}

// expr:
//   | query   { $1 }
//   | assert  { $1 }
define pExprRef { return! pQuery <|> pAssert }

// goal:
//   | QUERY clause PERIOD { Query $2 }
define pQueryRef {
  return! tQuery >>. pClause .>> tPeriod |>> (fun (clause) -> Syntax.Query(clause))
}

// assert:
//   | atom PERIOD               { Assert ($1, []) }
//   | atom ASSERT clause PERIOD { Assert ($1, $3) }
define pAssertRef {
  let pAssertionAtom = pAtom .>> tPeriod |>> (fun atom -> Syntax.Assert(atom, []))

  let pAssertionClause =
    pAtom .>> spaced tAssert .>>. pClause .>> spaced tPeriod
    |>> (fun (atom, clause) -> Syntax.Assert(atom, clause))

  return! pAssertionAtom <|> pAssertionClause
}

// atom:
//   | CONST                    { ($1, []) }
//   | CONST LPAREN args RPAREN { ($1, $3) }
define pAtomRef {
  let pAtomArgless = pConstant |>> (fun constant -> (constant, []))

  let pAtomArgful =
    pConstant .>> tLeftParen .>>. pArgs .>> tRightParen
    |>> (fun (constant, args) -> (constant, args))

  return! pAtomArgful <|> pAtomArgless
}

// clause:
//   | TRUE              { [] }
//   | atom              { [$1] }
//   | atom COMMA clause { $1 :: $3 }
define pClauseRef {
  let pTrue = spaced tTrue >>% []
  let pAtomSingle = pAtom |>> (fun atom -> [ atom ])

  let pAtomList =
    spaced pAtom .>> tComma .>>. spaced pClause
    |>> (fun (atom, clause) -> atom :: clause)

  return! pAtomList <|> pAtomSingle <|> pTrue
}

// args:
//   | literal            { [$1] }
//   | literal COMMA args { $1 :: $3 }
define pArgsRef {
  let pArgument = spaced pLiteral |>> (fun literal -> [ literal ])

  let pArguments =
    spaced pLiteral .>> tComma .>>. pArgs
    |>> (fun (literal, args) -> literal :: args)

  return! pArguments <|> pArgument
}

// literal:
//   | CONST                    { Const $1 }
//   | VAR                      { Var ($1, 0) }
//   | CONST LPAREN args RPAREN { App ($1, $3) }
define pLiteralRef {
  let pConst = pConstant |>> Syntax.Const

  let pVar = pVariable |>> (fun var -> Syntax.Var(var, 0))

  let pApp =
    pConstant .>>. between tLeftParen pArgs tRightParen
    |>> (fun (constant, args) -> Syntax.App(constant, args))

  return! pApp <|> pConst <|> pVar
}

// ---------------------------------------------------------------------------------------------------------------------
// Examples:

execute
  pFile
  """
    female(leia).
    male(vader).
    male(luke).
    male(kylo).

    child(luke, vader).
    child(leia, vader).
    child(kylo, leia).

    son(X,Y) :- male(X), child(X,Y).
    daughter(X,y) :- female(X), child(X,Y).

    grandchild(X,Z) :- child(X,Y), child(Y,Z).
  """
