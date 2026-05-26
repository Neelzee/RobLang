module RobLang.Runtime.SymbolTable

open RobLang.Ast

type FnBody =
  | Interpreted of Block
  | Native of string

type SymbolTable =
  { outer     : SymbolTable option
  ; variables : Map<string, Expr>
  ; functions : Map<string, FnDef>
  }

and FnDef =
  { parameters : Param list
  ; body       : FnBody
  ; closure    : SymbolTable
  }

let rec mkSymbolTable : SymbolTable =
  { outer     = None
  ; variables = Map.empty
  ; functions = Map.empty
  }

let mkSymbolTableInner (outer : SymbolTable) : SymbolTable =
  { mkSymbolTable with outer = Some outer }

let addVariable (st : SymbolTable) (id : string) (expr : Expr) : SymbolTable =
  { st with variables = Map.add id expr st.variables }

let removeVariable (st : SymbolTable) (id : string) : SymbolTable =
  { st with variables = Map.remove id st.variables }

let rec lookupVariable (id : string) (st : SymbolTable) : Expr option =
  Map.tryFind id st.variables
  |> Option.orElseWith (fun () -> st.outer |> Option.bind (lookupVariable id))

// Walks the scope chain to mutate an existing binding. Returns None if not found.
let rec setVariable (id : string) (value : Expr) (st : SymbolTable) : SymbolTable option =
  if Map.containsKey id st.variables then
    Some { st with variables = Map.add id value st.variables }
  else
    match st.outer with
    | Some outer ->
      setVariable id value outer
      |> Option.map (fun outer' -> { st with outer = Some outer' })
    | None -> None

let addFunction (name : string) (def : FnDef) (st : SymbolTable) : SymbolTable =
  { st with functions = Map.add name def st.functions }

let rec lookupFunction (name : string) (st : SymbolTable) : FnDef option =
  Map.tryFind name st.functions
  |> Option.orElseWith (fun () -> st.outer |> Option.bind (lookupFunction name))
