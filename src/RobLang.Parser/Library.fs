module RobLang.Parser.Core

open FParsec
open RobLang.Ast
open RobLang.Parser.Keyword

// ── Forwarded refs ────────────────────────────────────────────────────────────

let expr, exprRef = createParserForwardedToRef<Expr, unit> ()
let stmt, stmtRef = createParserForwardedToRef<Stmt, unit> ()

// ── Primitives ────────────────────────────────────────────────────────────────

// Float must require a decimal point so that 1..9 parses as Int 1, not Float 1.0
let num : Parser<Expr, unit> =
  attempt (
    many1Chars digit .>> pchar '.' .>>. many1Chars digit
    |>> fun (i, f) ->
      Float (System.Double.Parse($"{i}.{f}", System.Globalization.CultureInfo.InvariantCulture)))
  <|> (pint32 |>> Int)

let str : Parser<Expr, unit> =
  between (pchar '"') (pchar '"') (manyChars (noneOf ['"']))
  |>> String

let bool : Parser<Expr, unit> =
  attempt (pstring "true"  .>> notFollowedBy (letter <|> digit <|> pchar '_') >>% Bool true)
  <|> attempt (pstring "false" .>> notFollowedBy (letter <|> digit <|> pchar '_') >>% Bool false)

let ident : Parser<string, unit> =
  many1Chars2 letter (letter <|> digit <|> pchar '_')

// ── Range literal  [start..stop]  or  [start..step..stop] ────────────────────
// Tried before array so [0..9] is not mis-parsed as a one-element array.

let rangeLiteral : Parser<Expr, unit> =
  attempt (
    pchar '[' >>. spaces
    >>. pipe3
          (expr .>> spaces)
          (pstring ".." >>. spaces >>. expr .>> spaces)
          (opt (pstring ".." >>. spaces >>. expr .>> spaces))
          (fun start mid tail ->
            match tail with
            | None      -> Range (start, mid, None)
            | Some step -> Range (start, mid, Some step))
    .>> pchar ']')

// ── Array literal ─────────────────────────────────────────────────────────────

let array : Parser<Expr, unit> =
  list (pchar '[') (pchar ']') expr |>> Array

// ── Function call ─────────────────────────────────────────────────────────────

let fnCallRaw : Parser<string * Expr list, unit> =
  attempt (ident .>>. list (pchar '(') (pchar ')') expr)

let fnCall : Parser<Expr, unit> =
  fnCallRaw |>> Call

// ── Robot expressions  robot.method(args)  /  robot.field ────────────────────

let robCallExpr : Parser<Expr, unit> =
  attempt (robStmtKw >>. robAccessKw >>. fnCallRaw)
  |>> RobCall

let robGetExpr : Parser<Expr, unit> =
  attempt (robStmtKw >>. robAccessKw >>. ident)
  |>> RobGet

// ── Operator precedence parser ────────────────────────────────────────────────

let opp = OperatorPrecedenceParser<Expr, unit, unit>()

let atomicExpr : Parser<Expr, unit> =
  choice
    [ robCallExpr
      robGetExpr
      fnCall
      bool
      ident |>> Var
      num
      str
      rangeLiteral
      array
      parens opp.ExpressionParser
    ]

// Consume trailing whitespace as part of every term so the OPP sees
// operators immediately without leading spaces.
opp.TermParser <- atomicExpr .>> spaces

let private addInfix assoc prec sym ast =
  opp.AddOperator(InfixOperator(sym, spaces, prec, assoc, fun l r -> Op (Infix (l, ast, r))))

let private addPrefix prec sym (afterParser : Parser<unit, unit>) ast =
  opp.AddOperator(PrefixOperator(sym, afterParser, prec, true, fun e -> Op (Prefix (ast, e))))

do
  addInfix  Associativity.Left  1 "or"  Or
  addInfix  Associativity.Left  2 "and" And
  addInfix  Associativity.Left  3 "==" Eq
  addInfix  Associativity.Left  3 "!=" Neq
  addInfix  Associativity.Left  4 "<=" Lte
  addInfix  Associativity.Left  4 ">=" Gte
  addInfix  Associativity.Left  4 "<"  Lt
  addInfix  Associativity.Left  4 ">"  Gt
  addInfix  Associativity.Left  5 "+"  Add
  addInfix  Associativity.Left  5 "-"  Sub
  addInfix  Associativity.Left  6 "*"  Mul
  addInfix  Associativity.Left  6 "/"  Div
  addInfix  Associativity.Left  6 "%"  Mod
  addInfix  Associativity.Right 7 "**" Pow
  addPrefix                     8 "-"  spaces Neg
  // "not" must not match identifiers starting with "not" (e.g. "nothing")
  addPrefix                     8 "not" (notFollowedBy (letter <|> digit <|> pchar '_') >>. spaces) Not

// ── Blocks ────────────────────────────────────────────────────────────────────
// Blocks are terminated by an explicit keyword so greedy parsing doesn't
// swallow subsequent statements at the outer level.

// Used by if/else: first branch ends at "else", second at "end"
let blockUntilElse : Parser<Block, unit> =
  many (spaces >>. stmt .>> spaces) .>> (spaces >>. elseKw)

// Standard block: ends at "end"
let block : Parser<Block, unit> =
  many (spaces >>. stmt .>> spaces) .>> (spaces >>. endBlockKw)

// ── Statements ────────────────────────────────────────────────────────────────

let varDecl : Parser<Stmt, unit> =
  attempt (letKw >>. spaces1 >>. ident)
  .>>. (spaces >>. assignKw >>. spaces >>. opp.ExpressionParser)
  |>> VarDecl

let varAssDecl : Parser<Stmt, unit> =
  attempt (ident .>> spaces .>> assignKw .>> spaces .>>. opp.ExpressionParser)
  |>> VarAssDecl

let fnCallStmt : Parser<Stmt, unit> =
  fnCallRaw |>> FnCall

let fnDecl : Parser<Stmt, unit> =
  let param =
    ident .>>. opt (spaces >>. assignKw >>. spaces >>. opp.ExpressionParser)
  attempt (fnKw >>. spaces1 >>. ident)
  .>>. list (pchar '(') (pchar ')') param
  .>> spaces .>> pchar ':' .>> spaces
  .>>. block
  |>> fun ((name, parms), body) -> FnDecl (name, parms, body)

let ifElseStmt : Parser<Stmt, unit> =
  attempt (
    ifKw >>. spaces >>. parens opp.ExpressionParser
    .>> spaces .>> pchar ':' .>> spaces
    .>>. blockUntilElse
    .>> spaces .>> pchar ':' .>> spaces
    .>>. block
    |>> fun ((cond, b1), b2) -> IfElse (cond, b1, b2))

let ifStmt : Parser<Stmt, unit> =
  attempt (ifKw >>. spaces >>. parens opp.ExpressionParser)
  .>> spaces .>> pchar ':' .>> spaces
  .>>. block
  |>> If

let whileStmt : Parser<Stmt, unit> =
  attempt (whileKw >>. spaces >>. parens opp.ExpressionParser)
  .>> spaces .>> pchar ':' .>> spaces
  .>>. block
  |>> While

let forStmt : Parser<Stmt, unit> =
  attempt (
    forKw >>. spaces
    >>. parens (spaces >>. letKw >>. spaces1 >>. ident
                .>> spaces .>> inKw .>> spaces
                .>>. opp.ExpressionParser .>> spaces))
  .>> spaces .>> pchar ':' .>> spaces
  .>>. block
  |>> fun ((var, iter), body) -> For (var, iter, body)

let robCallStmt : Parser<Stmt, unit> =
  attempt (robStmtKw >>. robAccessKw >>. fnCallRaw)
  |>> RobCallStmt

let robAssStmt : Parser<Stmt, unit> =
  attempt (
    robStmtKw >>. robAccessKw >>. ident
    .>> spaces .>> assignKw .>> spaces
    .>>. opp.ExpressionParser)
  |>> RobAssStmt

let returnStmt : Parser<Stmt, unit> =
  attempt returnKw >>. spaces >>. opt opp.ExpressionParser
  |>> Return

let breakStmt : Parser<Stmt, unit> =
  attempt breakKw >>% Break

// ── Wire up forwarded refs ────────────────────────────────────────────────────

do exprRef.Value <- opp.ExpressionParser

do
  stmtRef.Value <-
    choice
      [ varDecl
        fnDecl
        ifElseStmt
        ifStmt
        whileStmt
        forStmt
        robCallStmt
        robAssStmt
        returnStmt
        breakStmt
        fnCallStmt
        varAssDecl
      ]

// ── Entry point ───────────────────────────────────────────────────────────────

let program : Parser<Program, unit> =
  spaces >>. many1 (spaces >>. stmt .>> spaces) .>> eof

let parse (source: string) : Result<Program, string> =
  match run program source with
  | Success (ast, _, _) -> Result.Ok ast
  | Failure (msg, _, _) -> Result.Error msg
