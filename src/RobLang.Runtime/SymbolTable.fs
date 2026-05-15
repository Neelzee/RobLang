module RobLang.Runtime.SymbolTable

open RobLang.Ast

type Params = Param list

type SymbolTable = 
  { outer : SymbolTable option
  ; variables : Map<string, Expr>
  ; functions : Map<string * Params, Block>
  }

let mkSymbolTable : SymbolTable =
  { outer = None; variables = Map.empty; functions = Map.empty }

let mkSymbolTableInner (outer : SymbolTable) : SymbolTable =
  { mkSymbolTable with outer = Some outer }

let addVariable (st : SymbolTable) (id : string) (expr : Expr) : SymbolTable =
  { st with variables = st.variables.Add (id, expr) }

let removeVariable (st : SymbolTable) (id : string) : SymbolTable =
  { st with variables = st.variables.Remove id }

let rec lookupVariable (id : string) (st : SymbolTable) : Expr option =
  Map.tryFind id st.variables
  |> Option.orElseWith (fun () -> st.outer |> Option.bind (lookupVariable id))

let addFunction (st : SymbolTable) (id : string * Params) (body : Block) : SymbolTable =
  { st with functions = st.functions.Add (id, body) }

let rec lookupFunction (id : string * Params) (st : SymbolTable) : Block option =
  Map.tryFind id st.functions
  |> Option.orElseWith (fun () -> st.outer |> Option.bind (lookupFunction id))

let rec lookupFunctionId
  (id : string)
  (st : SymbolTable)
  : (Params * Block) option =
  match Map.tryFindKey (fun (id', _) _ -> id' = id) st.functions with
  | Some (id, parms) ->
    lookupFunction (id, parms) st
    |> Option.map (fun block -> parms, block)
  | None -> None