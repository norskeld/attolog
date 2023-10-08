open Attolog.Parser

let execute p input =
  let result = run p input
  let output = ParserResult.toString result

  printfn "%A" output
  printfn "---------"

let helloWorld =
  pstring "Hello" .>> pwhitespace .>>. pstring "World"
  |>> fun (first, second) -> $"{first}! {second}?!"

execute helloWorld "Hello World"
execute helloWorld "Hello    World"
execute helloWorld "Hello        World"
