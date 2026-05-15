module Roblang.Runtime.Analyzer.Semantic

open RobLang.Ast
open RobLang.Runtime.SymbolTable

type ScopeKind =
  | ForScope
  | IfScope
  | WhileScope
  | FunctionScope
  | OuterScope

type SymbolTableError =
  | VarShadowing of string
  | MissingVariableDeclaration of string
  | MissingFunctionDeclaration of string
  | MissingArgument of string
  | InvalidUseOfBreakStmt
  | InvalidUseOfReturnStmt

let rec checkExpr
  (expr : Expr)
  (st : SymbolTable)
  : Result<SymbolTable, SymbolTableError> =
  match expr with  
  | Array xs ->
    List.fold
      (fun st cur ->
        match st with
        | Ok st' -> checkExpr cur st'
        | Error err -> Error err
      ) (Ok st) xs
  | Var id ->
    if Option.isNone (lookupVariable id st) then
      Error (MissingVariableDeclaration id)
    else
      Ok st
  | Call(id, _) ->
    if Option.isNone (lookupFunctionId id st) then
      Error (MissingFunctionDeclaration id)
    else
      Ok st
  | _ -> Ok st

let rec checkStmt
  (sks : ScopeKind list)
  (st : SymbolTable)
  (stmt : Stmt)
  : Result<SymbolTable, SymbolTableError> =
  match stmt, sks with  
  | VarDecl(id, expr), _ ->
    addVariable st id expr
    |> checkExpr expr
  | FnDecl(id, parms, block), _ -> addFunction st (id, parms) block |> Ok  
  | If(_, body), _ ->
    List.fold
      (fun acc cur ->
        match acc with
        | Ok st -> checkStmt (IfScope :: sks) st cur
        | Error err -> Error err
      )
      (Ok (mkSymbolTableInner st))
      body
  | While(cond, body), _ ->
    List.fold
      (fun acc cur ->
        match acc with
        | Ok st -> checkStmt (WhileScope :: sks) st cur
        | Error err -> Error err
      )
      (Ok (mkSymbolTableInner st))
      body
    |> Result.bind (checkExpr cond)
  | IfElse(cond, branch1, branch2), _ ->
    let result1 =
      List.fold
        (fun acc cur ->
          match acc with
          | Ok st -> checkStmt (IfScope :: sks) st cur
          | Error err -> Error err
        )
        (Ok (mkSymbolTableInner st))
        branch1
    let result2 =
      List.fold
        (fun acc cur ->
          match acc with
          | Ok st -> checkStmt (IfScope :: sks) st cur
          | Error err -> Error err
        )
        (Ok (mkSymbolTableInner st))
        branch2
    match result1, result2 with
    | Ok st, Ok _ -> checkExpr cond st
    | Error err, _ -> Error err
    | _, Error err -> Error err
  | For(_, iter, body), _ ->
    List.fold
      (fun acc cur ->
        match acc with
        | Ok st -> checkStmt (ForScope :: sks) st cur
        | Error err -> Error err
      )
      (Ok (mkSymbolTableInner st))
      body
    |> Result.bind (checkExpr iter)
  | FnCall(id, args), _ ->
    match lookupFunctionId id st with
    | Some (parms, body) ->
      let listLenDiff = parms.Length - args.Length
      if listLenDiff < 0 then
        Error (MissingFunctionDeclaration id)
      else
        let parameters =
          List.append (List.map Some args) (List.replicate listLenDiff None)
          |> List.zip parms
        let fnSt: Result<SymbolTable, SymbolTableError> = 
          List.fold (fun st' ((id, opt), expr) ->
            st'
            |> Result.bind (fun st'' ->
              match expr, opt with
              | Some e, _ | _, Some e ->
                addVariable st'' id e
                |> checkExpr e
              | None, None -> Error (MissingArgument id)
            )
          ) (Ok (mkSymbolTableInner st)) parameters
        List.fold
          (fun acc cur ->
            match acc with
            | Ok st -> checkStmt (IfScope :: sks) st cur
            | Error err -> Error err
          )
          fnSt
          body
    | None -> Error (MissingFunctionDeclaration id)
  | Break, sks ->
    let hasValidScope =
      List.fold (fun acc cur ->
        acc || cur = ForScope || cur = WhileScope
      ) false sks
    if hasValidScope then
      Ok st
    else
      Error InvalidUseOfBreakStmt
  | Return expr, sks ->
    if List.contains FunctionScope sks then
      match expr with      
      | Some v -> checkExpr v st
      | None -> Ok st
    else
      Error InvalidUseOfReturnStmt
 
let buildSymbolTable (program : Program) : Result<SymbolTable, SymbolTableError> =
  List.fold (fun st cur ->
    Result.bind (fun st' -> checkStmt [OuterScope] st' cur) st
  ) (Ok mkSymbolTable) program