open System

open Attolog
open Attolog.Parser
open Attolog.Prelude

let execute p input =
  let result = run p input
  let output = ParserResult.toString result

  printfn "%A" output

// ---------------------------------------------------------------------------------------------------------------------
// Terminals:

let pAssert = pstring ":-"
let pQuery = pstring "?-"
let pTrue = pstring "true"
let pFalse = pstring "false"
let pLeftParen = pstring "("
let pRightParen = pstring ")"
let pComma = pstring ","
let pPeriod = pstring "."

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

let pArgs, pArgsRef = createForwardedParser<list<Syntax.Term>> ()
let pLiteral, pLiteralRef = createForwardedParser<Syntax.Term> ()

// ---------------------------------------------------------------------------------------------------------------------
// Definitions:

do pArgsRef :=
  let pArgsSingle = pLiteral |>> (fun literal -> [ literal ]) in
  let pArgsMultiple = pLiteral .>> pComma .>>. pArgs |>> (fun (literal, args) -> literal :: args) in
  pArgsMultiple <|> pArgsSingle

do
  pLiteralRef
  := let pConst = pConstant |>> Syntax.Const in
     let pVar = pVariable |>> (fun var -> Syntax.Var(var, 0)) in

     let pApp =
       pConstant .>>. between pLeftParen pArgs pRightParen
       |>> (fun (constant, args) -> Syntax.App(constant, args)) in

     pApp <|> pConst <|> pVar

// ---------------------------------------------------------------------------------------------------------------------
// Examples:

execute pLiteral "hello"
execute pLiteral "HelloWorld"
execute pLiteral "hello(arg)"
execute pLiteral "hello(xarg,yarg)"
