[<AutoOpen>]
module Attolog.Cli

module Cli =
  type CliOptions = {
    /// File to load into the database.
    file: option<string>
  }

  type private FoldMode =
    | TopLevel
    | File

  type private FoldState = {
    options: CliOptions
    mode: FoldMode
  }

  let private parseTopLevel arg currentOptions =
    match arg with
    | "-f"
    | "--file" -> {
        options = currentOptions
        mode = File
      }
    | x ->
      if x.Length > 0 then
        printfn "Unrecognized argument: %s\n" x

      {
        options = currentOptions
        mode = TopLevel
      }

  let private parseFile arg currentOptions = {
    options = { currentOptions with file = Some(arg) }
    mode = TopLevel
  }

  let private parser (state: FoldState) element : FoldState =
    match state with
    | {
        options = currentOptions
        mode = TopLevel
      } -> parseTopLevel element currentOptions
    | {
        options = currentOptions
        mode = File
      } -> parseFile element currentOptions

  let parse args =
    let defaultOptions = { file = None }

    let initialState = {
      options = defaultOptions
      mode = TopLevel
    }

    (args |> List.fold parser initialState)
      .options
