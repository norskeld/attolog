module Attolog.Tests

open NUnit.Framework
open Attolog.Parser

[<Test>]
let ``Must successfully parse a character`` () =
  let actual = run (pchar 'a') "abc"
  let expected = Success('a', "bc")
  Assert.AreEqual(expected, actual)

[<Test>]
let ``Must successfully combine two parsers`` () =
  let actual = run (pchar 'a' .>>. pchar 'b') "abc"
  let expected = Success(('a', 'b'), "c")
  Assert.AreEqual(expected, actual)
