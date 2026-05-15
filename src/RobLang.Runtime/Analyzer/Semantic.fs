module Roblang.Runtime.Analyzer.Semantic

open RobLang.Ast
open RobLang.Runtime.SymbolTable

type SymbolTableError =
  | VarShadowing of string

let buildSymbolTable (program : Program) : Result<SymbolTable, SymbolTableError> =
  Ok mkSymbolTable