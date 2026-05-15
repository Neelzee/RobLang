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
  | MissingFunctionDeclaration of string
  | MissingArgument of string
  | InvalidUseOfBreakStmt
  | InvalidUseOfReturnStmt

let buildSymbolTable (program : Program) : Result<SymbolTable, SymbolTableError> =
  Ok mkSymbolTable

let rec checkStmt
  (sks : ScopeKind list)
  (st : SymbolTable)
  (stmt : Stmt)
  : Result<SymbolTable, SymbolTableError> =
  match stmt, sks with  
  | VarDecl(id, expr), _ -> addVariable st id expr |> Ok
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
  | While(_, body), _ ->
    List.fold
      (fun acc cur ->
        match acc with
        | Ok st -> checkStmt (WhileScope :: sks) st cur
        | Error err -> Error err
      )
      (Ok (mkSymbolTableInner st))
      body
  | IfElse(_, branch1, branch2), _ ->
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
    | Ok st, Ok _ -> Ok st
    | Error err, _ -> Error err
    | _, Error err -> Error err
  | For(_, _, body), _ ->
    List.fold
      (fun acc cur ->
        match acc with
        | Ok st -> checkStmt (ForScope :: sks) st cur
        | Error err -> Error err
      )
      (Ok (mkSymbolTableInner st))
      body
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
            match st' with
            | Ok st'' ->
              match expr, opt with
              | Some v, _ -> addVariable st'' id v |> Ok
              | _, Some o -> addVariable st'' id o |> Ok
              | None, None -> Error (MissingArgument id)
            | Error err -> Error err
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
  | Return _, sks ->
    if List.contains FunctionScope sks then
      Ok st
    else
      Error InvalidUseOfReturnStmt