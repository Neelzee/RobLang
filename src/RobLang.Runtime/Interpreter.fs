module RobLang.Runtime.Interpreter

open RobLang.Ast
open RobLang.Runtime.IHost
open RobLang.Runtime.SymbolTable
open RobLang.Runtime.Prelude.Builtins
open RobLang.Runtime.Analyzer.Semantic
open RobLang.Parser.Core

// ── Signal type ───────────────────────────────────────────────────────────────

type EvalSignal =
  | Value of Expr
  | ReturnSignal of Expr option
  | BreakSignal

type RuntimeError =
  | UndeclaredVariable of string
  | UndeclaredFunction of string
  | HostError         of string
  | TypeError          of string
  | BreakOutsideLoop
  | ReturnOutsideFunction
  | NativeNotFound     of string

// ── Helpers ───────────────────────────────────────────────────────────────────

let private evalArgs (st : SymbolTable) (host : IHost) (argExprs : Expr list) (evalExpr : SymbolTable -> IHost -> Expr -> Result<Expr, RuntimeError>) : Result<Expr list, RuntimeError> =
  List.foldBack
    (fun e acc ->
      match acc, evalExpr st host e with
      | Ok xs, Ok v -> Ok (v :: xs)
      | Error e, _  -> Error e
      | _, Error e  -> Error e)
    argExprs
    (Ok [])

// ── Expression evaluation ─────────────────────────────────────────────────────

let rec evalExpr (st : SymbolTable) (host : IHost) (expr : Expr) : Result<Expr, RuntimeError> =
  match expr with
  | Null | Int _ | Float _ | Bool _ | String _ -> Ok expr
  | Var id ->
    match lookupVariable id st with
    | Some v -> Ok v
    | None   -> Error (UndeclaredVariable id)
  | Array xs ->
    evalArgs st host xs evalExpr
    |> Result.map Array
  | Range (startE, stopE, stepE) ->
    let evalInt e =
      evalExpr st host e
      |> Result.bind (function
        | Int n -> Ok n
        | _     -> Error (TypeError "range expects integers"))
    evalInt startE
    |> Result.bind (fun start ->
      evalInt stopE
      |> Result.bind (fun stop ->
        match stepE with
        | None ->
          Ok (Array [ for i in start .. stop - 1 -> Int i ])
        | Some s ->
          evalInt s
          |> Result.map (fun step ->
            Array [ for i in start .. step .. stop - 1 -> Int i ])))
  | Op (Prefix (Neg, e)) ->
    evalExpr st host e
    |> Result.bind (function
      | Int n   -> Ok (Int -n)
      | Float f -> Ok (Float -f)
      | _       -> Error (TypeError "negation requires a number"))
  | Op (Prefix (Not, e)) ->
    evalExpr st host e
    |> Result.bind (function
      | Bool b -> Ok (Bool (not b))
      | _      -> Error (TypeError "not requires a bool"))
  | Op (Infix (l, op, r)) ->
    evalExpr st host l
    |> Result.bind (fun lv ->
      evalExpr st host r
      |> Result.bind (evalBinOp op lv))
  | Op (Affix (e, Index idx)) ->
    evalExpr st host e
    |> Result.bind (fun v ->
      evalExpr st host idx
      |> Result.bind (fun i ->
        match v, i with
        | Array xs, Int n when n >= 0 && n < xs.Length ->
          Ok xs.[n]
        | String s, Int n when n >= 0 && n < s.Length ->
          Ok (String (string s.[n]))
        | Array _, Int n  -> Error (TypeError $"index {n} out of bounds")
        | String _, Int n -> Error (TypeError $"index {n} out of bounds")
        | _ -> Error (TypeError "index requires array/string and integer")))
  | Op (Affix (e, Slice (s, t, step))) ->
    evalExpr st host e
    |> Result.bind (fun v ->
      let evalOptInt opt def =
        match opt with
        | None   -> Ok def
        | Some x ->
          evalExpr st host x
          |> Result.bind (function
            | Int n -> Ok n
            | _     -> Error (TypeError "slice indices must be integers"))
      let len =
        match v with
        | Array xs -> xs.Length
        | String sv -> sv.Length
        | _         -> 0
      evalOptInt s 0
      |> Result.bind (fun si ->
        evalOptInt t len
        |> Result.bind (fun ti ->
          evalOptInt step 1
          |> Result.map (fun stepN ->
            match v with
            | Array xs ->
              Array [ for i in si .. stepN .. ti - 1 do if i < xs.Length then yield xs.[i] ]
            | String sv ->
              String (System.String [| for i in si .. stepN .. ti - 1 do if i < sv.Length then yield sv.[i] |])
            | _ -> Null))))
  | Call (name, argExprs) ->
    evalArgs st host argExprs evalExpr
    |> Result.bind (evalCall st host name)
  | RobCall (method, argExprs) ->
    evalArgs st host argExprs evalExpr
    |> Result.bind (fun args ->
      host.Call method args
      |> Result.mapError HostError)
  | RobGet field ->
    host.Get field |> Result.mapError HostError

and private evalBinOp (op : BinOp) (l : Expr) (r : Expr) : Result<Expr, RuntimeError> =
  match op, l, r with
  | Add, Int a,    Int b    -> Ok (Int   (a + b))
  | Add, Float a,  Float b  -> Ok (Float (a + b))
  | Add, Int a,    Float b  -> Ok (Float (float a + b))
  | Add, Float a,  Int b    -> Ok (Float (a + float b))
  | Add, String a, String b -> Ok (String (a + b))
  | Sub, Int a,    Int b    -> Ok (Int   (a - b))
  | Sub, Float a,  Float b  -> Ok (Float (a - b))
  | Sub, Int a,    Float b  -> Ok (Float (float a - b))
  | Sub, Float a,  Int b    -> Ok (Float (a - float b))
  | Mul, Int a,    Int b    -> Ok (Int   (a * b))
  | Mul, Float a,  Float b  -> Ok (Float (a * b))
  | Mul, Int a,    Float b  -> Ok (Float (float a * b))
  | Mul, Float a,  Int b    -> Ok (Float (a * float b))
  | Div, Int a,    Int b    -> Ok (Int   (a / b))
  | Div, Float a,  Float b  -> Ok (Float (a / b))
  | Div, Int a,    Float b  -> Ok (Float (float a / b))
  | Div, Float a,  Int b    -> Ok (Float (a / float b))
  | Mod, Int a,    Int b    -> Ok (Int   (a % b))
  | Pow, Int a,    Int b    -> Ok (Float (System.Math.Pow(float a, float b)))
  | Pow, Float a,  Float b  -> Ok (Float (System.Math.Pow(a, b)))
  | Pow, Int a,    Float b  -> Ok (Float (System.Math.Pow(float a, b)))
  | Pow, Float a,  Int b    -> Ok (Float (System.Math.Pow(a, float b)))
  | Eq,  a, b -> Ok (Bool (a = b))
  | Neq, a, b -> Ok (Bool (a <> b))
  | Lt,  Int a,    Int b    -> Ok (Bool (a < b))
  | Lt,  Float a,  Float b  -> Ok (Bool (a < b))
  | Gt,  Int a,    Int b    -> Ok (Bool (a > b))
  | Gt,  Float a,  Float b  -> Ok (Bool (a > b))
  | Lte, Int a,    Int b    -> Ok (Bool (a <= b))
  | Lte, Float a,  Float b  -> Ok (Bool (a <= b))
  | Gte, Int a,    Int b    -> Ok (Bool (a >= b))
  | Gte, Float a,  Float b  -> Ok (Bool (a >= b))
  | And, Bool a,   Bool b   -> Ok (Bool (a && b))
  | Or,  Bool a,   Bool b   -> Ok (Bool (a || b))
  | _ -> Error (TypeError $"unsupported operand types for {op}")

and private evalCall (st : SymbolTable) (host : IHost) (name : string) (args : Expr list) : Result<Expr, RuntimeError> =
  match lookupFunction name st with
  | None -> Error (UndeclaredFunction name)
  | Some def ->
    match def.body with
    | Native key ->
      match Map.tryFind key registry with
      | None   -> Error (NativeNotFound key)
      | Some f -> f host args |> Result.mapError HostError
    | Interpreted body ->
      // Bind provided args; fill remaining from defaults.
      // Include the function itself in its own call scope so recursion works.
      let nArgs = args.Length
      let baseScope = mkSymbolTableInner def.closure |> addFunction name def
      let callSt =
        def.parameters
        |> List.mapi (fun i (pName, defaultVal) ->
            let value =
              if i < nArgs then args.[i]
              else Option.defaultValue Null defaultVal
            pName, value)
        |> List.fold (fun s (n, v) -> addVariable s n v) baseScope
      evalBlock callSt host body
      |> Result.map (fun (signal, _) ->
        // Function scope mutations don't propagate to the caller.
        match signal with
        | ReturnSignal (Some v) -> v
        | ReturnSignal None     -> Null
        | Value v               -> v
        | BreakSignal           -> Null)

// Returns the final scope alongside the signal so callers can propagate
// outer-variable mutations (via setVariable) back across scope boundaries.
and evalBlock (st : SymbolTable) (host : IHost) (stmts : Stmt list) : Result<EvalSignal * SymbolTable, RuntimeError> =
  let rec loop st stmts =
    match stmts with
    | [] -> Ok (Value Null, st)
    | s :: rest ->
      evalStmt st host s
      |> Result.bind (fun (signal, st') ->
        match signal with
        | Value _ -> loop st' rest
        | ReturnSignal _ | BreakSignal -> Ok (signal, st'))
  loop st stmts

// ── Statement evaluation ──────────────────────────────────────────────────────

and evalStmt (st : SymbolTable) (host : IHost) (stmt : Stmt) : Result<EvalSignal * SymbolTable, RuntimeError> =
  // Extract the updated outer scope from a finished inner scope so that
  // mutations via setVariable are visible to the parent after the block runs.
  let propagate (innerSt : SymbolTable) =
    Option.defaultValue st innerSt.outer

  match stmt with
  | VarDecl (id, expr) ->
    evalExpr st host expr
    |> Result.map (fun v -> Value v, addVariable st id v)
  | VarAssDecl (id, expr) ->
    evalExpr st host expr
    |> Result.bind (fun v ->
      match setVariable id v st with
      | Some st' -> Ok (Value v, st')
      | None     -> Error (UndeclaredVariable id))
  | FnDecl (name, parms, body) ->
    let def = { parameters = parms; body = Interpreted body; closure = st }
    Ok (Value Null, addFunction name def st)
  | If (cond, body) ->
    evalExpr st host cond
    |> Result.bind (function
      | Bool true  ->
        evalBlock (mkSymbolTableInner st) host body
        |> Result.map (fun (signal, innerSt) -> signal, propagate innerSt)
      | Bool false -> Ok (Value Null, st)
      | _          -> Error (TypeError "if condition must be a bool"))
  | IfElse (cond, b1, b2) ->
    evalExpr st host cond
    |> Result.bind (function
      | Bool true  ->
        evalBlock (mkSymbolTableInner st) host b1
        |> Result.map (fun (signal, innerSt) -> signal, propagate innerSt)
      | Bool false ->
        evalBlock (mkSymbolTableInner st) host b2
        |> Result.map (fun (signal, innerSt) -> signal, propagate innerSt)
      | _          -> Error (TypeError "if condition must be a bool"))
  | While (cond, body) ->
    let rec loop outerSt =
      evalExpr outerSt host cond
      |> Result.bind (function
        | Bool true ->
          evalBlock (mkSymbolTableInner outerSt) host body
          |> Result.bind (fun (signal, innerSt) ->
            let outerSt' = Option.defaultValue outerSt innerSt.outer
            match signal with
            | BreakSignal    -> Ok (Value Null, outerSt')
            | ReturnSignal v -> Ok (ReturnSignal v, outerSt')
            | Value _        -> loop outerSt')
        | Bool false -> Ok (Value Null, outerSt)
        | _          -> Error (TypeError "while condition must be a bool"))
    loop st
  | For (var, iter, body) ->
    evalExpr st host iter
    |> Result.bind (function
      | Array xs ->
        let rec loop outerSt xs =
          match xs with
          | [] -> Ok (Value Null, outerSt)
          | x :: rest ->
            let innerSt = addVariable (mkSymbolTableInner outerSt) var x
            evalBlock innerSt host body
            |> Result.bind (fun (signal, finalInnerSt) ->
              let outerSt' = Option.defaultValue outerSt finalInnerSt.outer
              match signal with
              | BreakSignal    -> Ok (Value Null, outerSt')
              | ReturnSignal v -> Ok (ReturnSignal v, outerSt')
              | Value _        -> loop outerSt' rest)
        loop st xs
      | _ -> Error (TypeError "for-each requires an array"))
  | FnCall (name, argExprs) ->
    evalArgs st host argExprs evalExpr
    |> Result.bind (evalCall st host name)
    |> Result.map (fun v -> Value v, st)
  | RobCallStmt (method, argExprs) ->
    evalArgs st host argExprs evalExpr
    |> Result.bind (fun args ->
      host.Call method args
      |> Result.mapError HostError)
    |> Result.map (fun v -> Value v, st)
  | RobAssStmt (field, expr) ->
    evalExpr st host expr
    |> Result.bind (fun v ->
      host.Set field v
      |> Result.mapError HostError)
    |> Result.map (fun () -> Value Null, st)
  | Break  -> Ok (BreakSignal, st)
  | Return expr ->
    match expr with
    | None   -> Ok (ReturnSignal None, st)
    | Some e ->
      evalExpr st host e
      |> Result.map (fun v -> ReturnSignal (Some v), st)

// ── Public API ────────────────────────────────────────────────────────────────

let eval (program : CheckedProgram) (host : IHost) : Result<unit, RuntimeError> =
  evalBlock prelude host (unwrap program)
  |> Result.map (fun _ -> ())

let repl (host : IHost) =
  printfn "(:q to quit)"
  let rec loop st =
    try
      printf "> "
      let input = System.Console.ReadLine()
      match input.Trim() with
      | ":q" | ":Q" -> ()
      | input' ->
        match parse input' with
        | Ok program ->
          match evalBlock st host program with
          | Ok (Value v, st') when v <> Null -> printfn "%A" v; loop st'
          | Ok (_, st')                      -> loop st'
          | Error e                          -> eprintfn $"Runtime error: {e}"; loop st
        | Error e ->
          eprintfn $"Parse error: {e}"; loop st
    with ex ->
      eprintfn $"Exception: {ex.Message}"; loop st
  loop prelude
