module RobLang.Tests.HelloWorld

open NUnit.Framework
open RobLang.Parser.Core
open RobLang.Ast

[<SetUp>]
let Setup () =
  ()

[<Test>]
let Test1 () =
  let p = parse $"print(\"Hello, World!\")"
  match p with
  | Ok r ->
    Assert.That(r.Head, Is.EqualTo (FnCall ("print", [String "Hello, World!"])))
  | Error err -> Assert.Fail $"Failed: {err}"
