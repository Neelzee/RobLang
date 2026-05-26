module RobLang.Cli.Main

open RobLang.Parser.Core
open RobLang.Runtime.Analyzer.Semantic
open RobLang.Runtime.Interpreter
open RobLang.Runtime.IHost
open RobLang.Ast

type CliHost() =
  let mutable fields : Map<string, Expr> = Map.empty

  interface IHost with
    member _.Print msg =
      printfn "%s" msg
      Ok ()

    member _.Get field =
      match fields.TryGetValue field with
      | true, v -> Ok v
      | _       -> Ok Null

    member _.Set field value =
      fields <- Map.add field value fields
      Ok ()

    member _.Call method args =
      printfn $"[host] {method}({args})"
      Ok Null

let replFlag = "--repl"

type CliProgramPartial =
  { replFlag   : bool option
  ; filePath   : string option
  ; outputPath : string option
  }

let mkCliProgramPartial : CliProgramPartial =
  { replFlag   = None
  ; filePath   = None
  ; outputPath = None
  }

type CliProgram =
  { replFlag   : bool
  ; filePath   : string option
  ; outputPath : string option
  }

let fromCliProgramPartial (cpp : CliProgramPartial) : CliProgram =
  { replFlag   = Option.defaultValue false cpp.replFlag
  ; filePath   = cpp.filePath
  ; outputPath = cpp.outputPath
  }

let mkCliProgram (xs : string array) : CliProgram =
  let rec helper (ys : string list) (cpp : CliProgramPartial) : CliProgramPartial =
    match ys with
    | [] -> cpp
    | x :: rest when x = replFlag     -> helper rest { cpp with replFlag = Some true }
    | a :: b :: rest when a = "-i"    -> helper rest { cpp with filePath = Some b }
    | a :: b :: rest when a = "-o"    -> helper rest { cpp with outputPath = Some b }
    | x :: rest ->
      eprintfn $"Unknown flag: {x}"
      helper rest cpp
  helper (Array.toList xs) mkCliProgramPartial
  |> fromCliProgramPartial

let execCliProgram (cli : CliProgram) =
  let host = CliHost() :> IHost
  match cli.replFlag, cli.filePath with
  | true, Some fp ->
    try
      match System.IO.File.ReadAllText fp |> parse with
      | Ok program ->
        match check program with
        | Ok cp ->
          match eval cp host with
          | Ok ()   -> repl host
          | Error e -> eprintfn $"Runtime error: {e}"; repl host
        | Error e ->
          eprintfn $"Semantic error: {e}"; repl host
      | Error e ->
        eprintfn $"Parse error: {e}"; repl host
      0
    with ex ->
      eprintfn $"Exception: {ex}"; 1
  | true, _ ->
    repl host
    0
  | false, Some fp ->
    try
      match System.IO.File.ReadAllText fp |> parse with
      | Ok program ->
        match check program with
        | Ok cp ->
          match eval cp host with
          | Ok ()   -> 0
          | Error e -> eprintfn $"Runtime error: {e}"; 1
        | Error e ->
          eprintfn $"Semantic error: {e}"; 1
      | Error e ->
        eprintfn $"Parse error: {e}"; 1
    with ex ->
      eprintfn $"Exception: {ex}"; 1
  | _, None ->
    eprintfn "Usage: roblang [-i <file>] [--repl]"
    1

[<EntryPoint>]
let main argv =
  mkCliProgram argv |> execCliProgram
