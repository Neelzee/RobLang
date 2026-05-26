module RobLang.Tests.CliTests

open System
open System.IO
open NUnit.Framework
open RobLang.Cli.Main

// ── Helpers ───────────────────────────────────────────────────────────────────

let private withTempRl (source : string) (f : string -> 'a) =
  let path = Path.ChangeExtension(Path.GetTempFileName(), ".rl")
  File.WriteAllText(path, source)
  try   f path
  finally File.Delete path

let private captureStderr (f : unit -> 'a) =
  let writer   = new StringWriter()
  let original = Console.Error
  Console.SetError writer
  try
    let result = f ()
    Console.SetError original
    result, writer.ToString()
  finally
    Console.SetError original

// ── Arg parsing ───────────────────────────────────────────────────────────────

[<TestFixture>]
type ArgParsingTests() =

  [<Test>]
  member _.ParsesReplFlag() =
    let cp = mkCliProgram [| "--repl" |]
    Assert.That(cp.replFlag, Is.True)
    Assert.That(cp.filePath, Is.EqualTo(None))

  [<Test>]
  member _.ParsesInputFile() =
    let cp = mkCliProgram [| "-i"; "foo.rl" |]
    Assert.That(cp.replFlag, Is.False)
    Assert.That(cp.filePath, Is.EqualTo(Some "foo.rl"))

  [<Test>]
  member _.ParsesReplWithFile() =
    let cp = mkCliProgram [| "--repl"; "-i"; "foo.rl" |]
    Assert.That(cp.replFlag, Is.True)
    Assert.That(cp.filePath, Is.EqualTo(Some "foo.rl"))

  [<Test>]
  member _.DefaultsWhenNoArgs() =
    let cp = mkCliProgram [||]
    Assert.That(cp.replFlag, Is.False)
    Assert.That(cp.filePath, Is.EqualTo(None))

// ── File execution ────────────────────────────────────────────────────────────

[<TestFixture>]
type ExecTests() =

  [<Test>]
  member _.ReturnsZeroForValidFile() =
    withTempRl "print(1 + 2)" (fun path ->
      let code = execCliProgram (mkCliProgram [| "-i"; path |])
      Assert.That(code, Is.EqualTo 0))

  [<Test>]
  member _.ReturnsOneForParseError() =
    withTempRl "@@@ not valid" (fun path ->
      let _, stderr = captureStderr (fun () ->
        execCliProgram (mkCliProgram [| "-i"; path |]))
      Assert.That(stderr, Does.Contain("Parse error")))

  [<Test>]
  member _.ReturnsOneForRuntimeError() =
    // Division by zero causes a TypeError (0 / 0 is actually fine in F# int, try undefined var)
    withTempRl "print(undeclaredVar)" (fun path ->
      let code, _ = captureStderr (fun () ->
        execCliProgram (mkCliProgram [| "-i"; path |]))
      Assert.That(code, Is.EqualTo 1))

  [<Test>]
  member _.ReturnsOneForMissingFile() =
    let code, _ = captureStderr (fun () ->
      execCliProgram (mkCliProgram [| "-i"; "/nonexistent/path/prog.rl" |]))
    Assert.That(code, Is.EqualTo 1)

  [<Test>]
  member _.ReturnsOneWithNoArgs() =
    let code, _ = captureStderr (fun () ->
      execCliProgram (mkCliProgram [||]))
    Assert.That(code, Is.EqualTo 1)

  [<Test>]
  member _.RobotFieldsAreStateful() =
    withTempRl "robot.x = 10\nrobot.x = 20" (fun path ->
      let code = execCliProgram (mkCliProgram [| "-i"; path |])
      Assert.That(code, Is.EqualTo 0))

  [<Test>]
  member _.RobotFieldReadBack() =
    withTempRl "robot.val = 7\nlet v = robot.val + 1\nprint(v)" (fun path ->
      let code = execCliProgram (mkCliProgram [| "-i"; path |])
      Assert.That(code, Is.EqualTo 0))
