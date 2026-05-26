module RobLang.Runtime.Prelude.Builtins

open RobLang.Ast
open RobLang.Runtime.IHost
open RobLang.Runtime.SymbolTable

// Native builtin implementations resolved at eval time.
// Keys match the Native(name) stored in FnBody.
let registry : Map<string, IHost -> Expr list -> Result<Expr, string>> =
  Map.ofList
    [ "print",
        (fun host args ->
          match args with
          | [String s] -> host.Print s |> Result.map (fun () -> Null)
          | [x]        -> host.Print (sprintf "%A" x) |> Result.map (fun () -> Null)
          | _          -> Error "print expects one argument")

      "sqrt",
        (fun _ args ->
          match args with
          | [Int n]   -> Ok (Float (sqrt (float n)))
          | [Float f] -> Ok (Float (sqrt f))
          | _         -> Error "sqrt expects one numeric argument")

      "range",
        (fun _ args ->
          match args with
          | [Int start; Int stop] ->
            Ok (Array [ for i in start .. stop - 1 -> Int i ])
          | [Int start; Int stop; Int step] ->
            Ok (Array [ for i in start .. step .. stop - 1 -> Int i ])
          | _ -> Error "range expects range(start, stop) or range(start, stop, step)") ]

let private nativeDef (name : string) (parms : Param list) : FnDef =
  { parameters = parms
  ; body       = Native name
  ; closure    = mkSymbolTable
  }

// Pre-loaded symbol table with all builtins registered.
let prelude : SymbolTable =
  mkSymbolTable
  |> addFunction "print" (nativeDef "print" [("msg", None)])
  |> addFunction "sqrt"  (nativeDef "sqrt"  [("n",   None)])
  |> addFunction "range" (nativeDef "range" [("start", None); ("stop", None); ("step", Some (Int 1))])
