module RobLang.Cli.Main

open RobLang.Parser.Core
open RobLang.Runtime.SymbolTable
open Roblang.Runtime.Analyzer.Semantic

let repl (st : SymbolTable) =
  printfn "(:q to quit)"
  RobLang.Runtime.Interpreter.repl st

let replFlag =
  "--repl"

type CliProgramPartial =
  { replFlag : bool option
  ; filePath : string option
  ; outputPath : string option
  }

let mkCliProgramPartial : CliProgramPartial =
  { replFlag = None
  ; filePath = None
  ; outputPath = None
  }

type CliProgram =
  { replFlag : bool
  ; filePath : string option
  ; outputPath : string option
  }

let fromCliProgramPartial (cpp : CliProgramPartial) : CliProgram =
  { replFlag = Option.defaultValue false cpp.replFlag
  ; filePath = cpp.filePath
  ; outputPath = cpp.outputPath
  }

let mkCliProgram (xs : string array) : CliProgram =
  let rec helper
    (ys : string list)
    (cpp : CliProgramPartial)
    : CliProgramPartial =
      match ys with
      | [] -> cpp
      | x :: xs when x = replFlag -> helper xs { cpp with replFlag = Some true }
      | a :: b :: xs when a = "-i" -> helper xs { cpp with filePath = Some b }
      | a :: b :: xs when a = "-o" -> helper xs { cpp with outputPath = Some b }
      | x :: xs ->
        eprintfn $"Unknown flag: {x}"
        helper xs cpp
  helper (Array.toList xs) mkCliProgramPartial
  |> fromCliProgramPartial


let execCliProgram (cli : CliProgram) =
  match cli.replFlag, cli.filePath with
  | true, Some fp ->
    try
      let st =
        System.IO.File.ReadAllText fp
        |> parse
        |> Result.mapError fromParseError
        |> Result.bind buildSymbolTable
      match st with
      | Ok st' -> repl st'
      | Error err ->
        eprintfn $"Failed to load repl with file content, due to error: {err}"
        repl mkSymbolTable
      0
    with
      | err ->
        eprintfn $"Exception: {err}"
        1
  | true, _ ->
    repl mkSymbolTable
    0
  | false, Some fp ->
    try
      let st =
        System.IO.File.ReadAllText fp
        |> parse
        |> Result.mapError fromParseError
        |> Result.bind buildSymbolTable
      match st with
      | Ok st' ->
        failwith "No interpreter"
        0
      | Error err ->
        eprintfn $"Failed to load repl with file content, due to error: {err}"
        1
    with
      | err ->
        eprintfn $"Exception: {err}"
        1
  | _, None ->
    eprintfn "Usage: roblang <file>"
    1

[<EntryPoint>]
let main argv =
  let cli = mkCliProgram argv
  execCliProgram cli