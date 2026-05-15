module RobLang.Runtime.SymbolTable

open RobLang.Ast

type SymbolTable = 
  { outer : SymbolTable option
  ; variables : Map<string, Expr>
  ; functions : Map<string, Block>
  }

let mkSymbolTable : SymbolTable =
  { outer = None; variables = Map.empty; functions = Map.empty }

let addVariable (st : SymbolTable) (id : string) (expr : Expr) : SymbolTable =
  { st with variables = st.variables.Add (id, expr) }

let removeVariable (st : SymbolTable) (id : string) : SymbolTable =
  { st with variables = st.variables.Remove id }

let rec lookupVariable (id : string) (st : SymbolTable) : Expr option =
  Map.tryFind id st.variables
  |> Option.orElseWith (fun () -> st.outer |> Option.bind (lookupVariable id))

let addFunction (st : SymbolTable) (id : string) (body : Block) : SymbolTable =
  { st with functions = st.functions.Add (id, body) }

let removeFunction (st : SymbolTable) (id : string) : SymbolTable =
  { st with functions = st.functions.Remove id }

let rec lookupFunction (id : string) (st : SymbolTable) : Block option =
  Map.tryFind id st.functions
  |> Option.orElseWith (fun () -> st.outer |> Option.bind (lookupFunction id))