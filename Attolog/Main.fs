module Attolog.Main

open System

open Attolog.Prelude
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

let introduction =
  let interrupt =
    match Environment.OSVersion.Platform with
    | PlatformID.Unix
    | PlatformID.MacOSX -> "Ctrl-D"
    | PlatformID.Win32NT -> "Ctrl-Z"
    | _ -> "Ctrl-C"

  let title = $"Attolog: Minimal Prolog interpreter.\n\n"
  let hint = $"Press {interrupt} to exit.\n"

  title + hint

let rec readlines () =
  seq {
    printf "atto> "

    let line = Console.ReadLine()

    if line <> null then
      yield line + "\n"
      yield! readlines ()
  }

[<EntryPoint>]
let main _ =
  printfn $"{introduction}"

  for lines in readlines () do
    match Parser.parse lines with
    | Ok commands ->
      for command in commands do
        Solver.solve command
    | Error message -> printfn "%s" message

  0
