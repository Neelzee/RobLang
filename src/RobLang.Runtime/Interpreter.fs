module RobLang.Runtime.Interpreter

open RobLang.Runtime.SymbolTable
open RobLang.Parser.Core
open Roblang.Runtime.Analyzer.Semantic

let rec repl (st : SymbolTable) =
  try
    let input = System.Console.ReadLine()
    match input.Trim() with
    | ":q" | ":Q" -> ()
    | input' ->
      match parse input' with
      | Ok program ->
        match updateSymbolTable program st with
        | Ok st' -> repl st'
        | Error error ->
          eprintfn $"Failed with error: {error}"
          repl st
      | Error error ->
        eprintfn $"Failed with error: {error}"
        repl st
  with
    | error ->
      eprintfn $"Failed with error: {error}"
      repl st