module Program

open System

[<EntryPoint>]
let main _ =
  let exitCode = 0

  Console.WriteLine("Press any key")
  Console.ReadLine() |> ignore

  exitCode
