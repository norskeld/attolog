module Attolog.Main

open Attolog.Core

let program =
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

[<EntryPoint>]
let main _ =
  match Parser.parse program with
  | Ok result -> printfn "%A" result
  | Error error -> printfn "%A" error

  0
