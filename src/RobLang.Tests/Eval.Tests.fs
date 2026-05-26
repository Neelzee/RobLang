module RobLang.Tests.EvalTests

open System.IO
open NUnit.Framework
open RobLang.Ast
open RobLang.Parser.Core
open RobLang.Runtime.IHost
open RobLang.Runtime.Analyzer.Semantic
open RobLang.Runtime.Interpreter

let private exampleDir =
  Path.Combine(__SOURCE_DIRECTORY__, "Example")

type StubHost() =
  let mutable fields : Map<string, Expr> = Map.empty
  interface IHost with
    member _.Print _ = Ok ()
    member _.Get field =
      match fields.TryGetValue field with
      | true, v -> Ok v
      | _       -> Ok Null
    member _.Set field value =
      fields <- Map.add field value fields
      Ok ()
    member _.Call _ _ = Ok Null

type RecordingHost() =
  let fields = System.Collections.Generic.Dictionary<string, Expr>()
  let mutable calls : Map<string, Expr list> = Map.empty

  member _.Calls  = calls
  member _.Fields = fields

  interface IHost with
    member _.Print _ = Ok ()

    member _.Get field =
      match fields.TryGetValue field with
      | true, v -> Ok v
      | _       -> Ok Null

    member _.Set field value =
      fields.[field] <- value
      Ok ()

    member _.Call method args =
      calls <-
        calls |> Map.change method (function
          | None      -> Some args
          | Some prev -> Some (prev @ args))
      Ok Null

[<Test>]
let RobotExampleComputes () =
  let filePath = $"{exampleDir}/Robot.rl"
  let name   = Path.GetFileName filePath
  let source = File.ReadAllText filePath
  let host   = RecordingHost()

  match parse source with
  | Error e -> Assert.Fail $"Parse error in {name}:\n{e}"
  | Ok program ->
    match check program with
    | Error e -> Assert.Fail $"Semantic error in {name}:\n{e}"
    | Ok cp   ->
      match eval cp (host :> IHost) with
      | Error e -> Assert.Fail $"Runtime error in {name}:\n{e}"
      | Ok ()   ->
        Assert.That(host.Calls["move"], Is.EqualTo<Expr list> [Int 1; Int 3])
        Assert.That((host :> IHost).Get "field", Is.EqualTo<Result<Expr, string>> (Ok (Int 42)))

[<TestFixture>]
type EvalTests() =
  static member ExampleFiles : TestCaseData seq =
    Directory.GetFiles(exampleDir, "*.rl")
    |> Array.map (fun path ->
      TestCaseData(path)
        .SetName(Path.GetFileNameWithoutExtension path))
    |> Array.toSeq

  [<TestCaseSource("ExampleFiles")>]
  member _.EvalWithoutError(filePath: string) =
    let name   = Path.GetFileName filePath
    let source = File.ReadAllText filePath
    let host   = StubHost() :> IHost

    match parse source with
    | Error e -> Assert.Fail $"Parse error in {name}:\n{e}"
    | Ok program ->
      match check program with
      | Error e -> Assert.Fail $"Semantic error in {name}:\n{e}"
      | Ok cp   ->
        match eval cp host with
        | Error e -> Assert.Fail $"Runtime error in {name}:\n{e}"
        | Ok ()   -> ()
