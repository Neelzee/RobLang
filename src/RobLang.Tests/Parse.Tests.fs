module RobLang.Tests.ParseTests

open System.IO
open NUnit.Framework
open RobLang.Parser.Core

let private exampleDir =
  Path.Combine(__SOURCE_DIRECTORY__, "Example")

[<TestFixture>]
type ParseTests() =

  static member ExampleFiles : TestCaseData seq =
    Directory.GetFiles(exampleDir, "*.rl")
    |> Array.map (fun path ->
      TestCaseData(path)
        .SetName(Path.GetFileNameWithoutExtension(path)))
    |> Array.toSeq

  [<TestCaseSource("ExampleFiles")>]
  member _.ParsesWithoutError(filePath: string) =
    let source = File.ReadAllText(filePath)
    match parse source with
    | Ok _    -> ()
    | Error e -> Assert.Fail $"Parse error in {Path.GetFileName(filePath)}:\n{e}"
