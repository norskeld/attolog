module Attolog.Main

open Attolog.Parser
open Attolog.Core.Parser

let execute p input =
  let result = run p input
  let output = ParserResult<_>.toString result

  printfn "%A" output

[<EntryPoint>]
let main _ =
  execute
    pDocument
    """
      -- Define persons.
      female(leia).
      male(vader).
      male(luke).
      male(kylo).

      -- Define relations.
      child(luke, vader).
      child(leia, vader).
      child(kylo, leia).

      son(X, Y) :- male(X), child(X, Y).
      daughter(X, Y) :- female(X), child(X, Y).

      grandchild(X, Z) :- child(X, Y), child(Y, Z).
    """

  0
