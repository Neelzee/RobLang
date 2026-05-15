module RobLang.Tests.HelloWorld

open NUnit.Framework
open RobLang.Parser.Core
open RobLang.Ast

[<SetUp>]
let Setup () =
  ()

[<Test>]
let HelloWorld () =
  let p = parse $"print(\"Hello, World!\")"
  match p with
  | Ok r ->
    Assert.That(r.Head, Is.EqualTo (FnCall ("print", [String "Hello, World!"])))
  | Error err -> Assert.Fail $"Failed: {err}"


[<Test>]
let LetDecl () =
  let p = parse $"let foo = \"Hello, World!\""
  match p with
  | Ok r ->
    Assert.That(r.Head, Is.EqualTo (VarDecl ("foo", String "Hello, World!")))
  | Error err -> Assert.Fail $"Failed: {err}"
