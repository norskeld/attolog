module Attolog.Main

open System

open Attolog.Cli
open Attolog.Core

/// Reads a file and returns its contents as a single string.
let readFile (file: string) : option<string> =
  try
    let loaded = IO.File.ReadAllText(file)
    printfn $"File loaded: {file}\n"
    Some loaded
  with
  | :? IO.FileNotFoundException ->
    printfn $"File not found: {file}\n"
    None
  | _ ->
    printfn $"Could not read file: {file}\n"
    None

/// Help message.
let introduction: string =
  let interrupt =
    match Environment.OSVersion.Platform with
    | PlatformID.Unix
    | PlatformID.MacOSX -> "Ctrl-D"
    | PlatformID.Win32NT -> "Ctrl-Z"
    | _ -> "Ctrl-C"

  let title = $"Attolog: Minimal Prolog interpreter.\n\n"
  let hint = $"Press {interrupt} to exit.\n"

  title + hint

/// Reads lines from stdin.
let rec readlines () : seq<string> =
  seq {
    printf "atto> "

    let line = Console.ReadLine()

    if line <> null then
      yield line + "\n"
      yield! readlines ()
  }

[<EntryPoint>]
let main args =
  printfn $"{introduction}"

  let options = Cli.parse (Array.toList args)

  // If given a file, read it, parse and populate the database so we can consult it via REPL.
  if options.file.IsSome then
    let file = readFile options.file.Value

    if file.IsSome then
      match Parser.parse file.Value with
      | Ok commands ->
        for command in commands do
          Solver.load command
      | Error message -> printfn $"{message}"

  // Start REPL.
  for lines in readlines () do
    match Parser.parse lines with
    | Ok commands ->
      for command in commands do
        Solver.solve command
    | Error message -> printfn $"{message}"

  0
