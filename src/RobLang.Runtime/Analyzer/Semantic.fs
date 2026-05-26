module RobLang.Runtime.Analyzer.Semantic

open RobLang.Ast
open RobLang.Runtime.SymbolTable

type ScopeKind =
  | ForScope
  | IfScope
  | WhileScope
  | FunctionScope
  | OuterScope

type SemanticError =
  | ParseError of string
  | VarShadowing of string
  | MissingVariableDeclaration of string
  | MissingFunctionDeclaration of string
  | MissingArgument of string
  | InvalidUseOfBreakStmt
  | InvalidUseOfReturnStmt

type CheckedProgram = private CheckedProgram of Program

let unwrap (CheckedProgram p) : Program = p

let fromParseError (msg : string) : SemanticError = ParseError msg

let private foldStmts
  (sks   : ScopeKind list)
  (st    : SymbolTable)
  (stmts : Stmt list)
  (f     : ScopeKind list -> SymbolTable -> Stmt -> Result<SymbolTable, SemanticError>)
  : Result<SymbolTable, SemanticError> =
  List.fold
    (fun acc cur -> Result.bind (fun st' -> f sks st' cur) acc)
    (Ok st)
    stmts

let rec private checkExpr
  (st   : SymbolTable)
  (expr : Expr)
  : Result<unit, SemanticError> =
  match expr with
  | Array xs ->
    List.fold
      (fun acc cur -> Result.bind (fun () -> checkExpr st cur) acc)
      (Ok ())
      xs
  | Range (start, stop, step) ->
    [ Some start; Some stop; step ]
    |> List.choose id
    |> List.fold (fun acc e -> Result.bind (fun () -> checkExpr st e) acc) (Ok ())
  | Var id ->
    if Option.isNone (lookupVariable id st) then
      Error (MissingVariableDeclaration id)
    else
      Ok ()
  | Call (_, args) ->
    List.fold
      (fun acc e -> Result.bind (fun () -> checkExpr st e) acc)
      (Ok ())
      args
  | Op (Prefix (_, e)) -> checkExpr st e
  | Op (Infix (l, _, r)) ->
    checkExpr st l |> Result.bind (fun () -> checkExpr st r)
  | Op (Affix (e, _)) -> checkExpr st e
  | _ -> Ok ()

let rec private checkStmt
  (sks  : ScopeKind list)
  (st   : SymbolTable)
  (stmt : Stmt)
  : Result<SymbolTable, SemanticError> =
  match stmt with
  | VarDecl (id, expr) ->
    checkExpr st expr
    |> Result.map (fun () -> addVariable st id expr)
  | VarAssDecl (id, expr) ->
    if Option.isNone (lookupVariable id st) then
      Error (MissingVariableDeclaration id)
    else
      checkExpr st expr
      |> Result.map (fun () -> st)
  | FnDecl (name, parms, body) ->
    let def =
      { parameters = parms
      ; body       = Interpreted body
      ; closure    = st
      }
    let fnSt = addFunction name def st
    let innerSt =
      List.fold
        (fun s (p, d) -> addVariable s p (Option.defaultValue Null d))
        (mkSymbolTableInner fnSt)
        parms
    foldStmts (FunctionScope :: sks) innerSt body checkStmt
    |> Result.map (fun _ -> fnSt)
  | If (cond, body) ->
    checkExpr st cond
    |> Result.bind (fun () ->
      foldStmts (IfScope :: sks) (mkSymbolTableInner st) body checkStmt)
    |> Result.map (fun _ -> st)
  | IfElse (cond, branch1, branch2) ->
    checkExpr st cond
    |> Result.bind (fun () ->
      foldStmts (IfScope :: sks) (mkSymbolTableInner st) branch1 checkStmt)
    |> Result.bind (fun _ ->
      foldStmts (IfScope :: sks) (mkSymbolTableInner st) branch2 checkStmt)
    |> Result.map (fun _ -> st)
  | While (cond, body) ->
    checkExpr st cond
    |> Result.bind (fun () ->
      foldStmts (WhileScope :: sks) (mkSymbolTableInner st) body checkStmt)
    |> Result.map (fun _ -> st)
  | For (var, iter, body) ->
    checkExpr st iter
    |> Result.bind (fun () ->
      let innerSt = addVariable (mkSymbolTableInner st) var Null
      foldStmts (ForScope :: sks) innerSt body checkStmt)
    |> Result.map (fun _ -> st)
  | FnCall (_, args) ->
    // Function existence is checked at runtime (dynamic typing; builtins live in prelude).
    List.fold
      (fun acc e -> Result.bind (fun () -> checkExpr st e) acc)
      (Ok ())
      args
    |> Result.map (fun () -> st)
  | RobCallStmt (_, args) ->
    List.fold
      (fun acc e -> Result.bind (fun () -> checkExpr st e) acc)
      (Ok ())
      args
    |> Result.map (fun () -> st)
  | RobAssStmt (_, value) ->
    checkExpr st value |> Result.map (fun () -> st)
  | Break ->
    if List.exists (fun sk -> sk = ForScope || sk = WhileScope) sks then
      Ok st
    else
      Error InvalidUseOfBreakStmt
  | Return expr ->
    if List.contains FunctionScope sks then
      match expr with
      | Some e -> checkExpr st e |> Result.map (fun () -> st)
      | None   -> Ok st
    else
      Error InvalidUseOfReturnStmt

let check (program : Program) : Result<CheckedProgram, SemanticError> =
  foldStmts [OuterScope] mkSymbolTable program checkStmt
  |> Result.map (fun _ -> CheckedProgram program)

let buildSymbolTable (program : Program) : Result<SymbolTable, SemanticError> =
  foldStmts [OuterScope] mkSymbolTable program checkStmt
