module RobLang.Parser.Core

open FParsec
open RobLang.Ast
open RobLang.Parser.Keyword

let expr, exprRef = createParserForwardedToRef<Expr, unit> ()

let num: Parser<Expr, unit> =
  pint32
  |>> Int

let str: Parser<Expr, unit> =
  let ap = pchar '"'
  between ap ap (manyChars (noneOf ['"']))
  |>> String

let bool: Parser<Expr, unit> =
  choice [
    pstring "true" >>% Bool true
    pstring "false" >>% Bool false
  ]

let array: Parser<Expr, unit> =
  list (pchar '[') (pchar ']') expr
  |>> Array

let float: Parser<Expr, unit> =
  pfloat
  |>> Float

let ident: Parser<string, unit> =
  many1Chars2 letter (letter <|> digit)

let varDeclRaw =
  consumeSpaces letKw >>. ident
  .>>.
  (consumeSpaces assignmentKw >>. expr)

let stmt, stmtRef = createParserForwardedToRef<Stmt, unit> ()

let block : Parser<Block, unit> = many1(consumeSpaces stmt)

let varDecl: Parser<Stmt, unit> =
  varDeclRaw
  |>> VarDecl

let nonRecStmt: Parser<Stmt, unit> =
  varDecl

let forStmt: Parser<Stmt, unit> = 
  consumeSpaces forKw >>. parens (consumeSpaces varDeclRaw .>> endStmtKw)
  .>>.
  (consumeSpaces forStartKw >>. newline >>. many1 (opt newline >>. stmt))
  |>> fun ((x, y), z) -> For (x, y, z)

let fnCallRaw =
  ident .>>. list (pchar '(') (pchar ')') expr

let fnCallStmt: Parser<Stmt, unit> =
  fnCallRaw
  |>> FnCall

let fnCall: Parser<Expr, unit> =
  fnCallRaw
  |>> Call

let ifStmt: Parser<Stmt, unit> =
  ifKw >>. spaces >>. parens expr .>> pstring ":" .>>. block
  |>> If

let whileStmt: Parser<Stmt, unit> =
  whileKw >>. spaces >>. parens expr .>> pstring ":" .>>. block
  |>> While

let ifElseStmt: Parser<Stmt, unit> =
  ifKw >>. spaces >>. parens expr .>> pstring ":" .>>. block .>>. elseKw .>>. block
  |>> fun (((cond, fstBranch), _), sndBranch) ->
    IfElse (cond, fstBranch, sndBranch)
  
let returnStmt: Parser<Stmt, unit> =
  returnKw >>. opt expr
  |>> Return

let breakStmt: Parser<Stmt, unit> =
  pstring "break" >>% Break

let program: Parser<Program, unit> =
  many1 stmt

do
  exprRef.Value <-
    choice [
      num;
      ident |>> Var;
      str;
      bool;
      float;
      array;
    ]

do
  stmtRef.Value <-
    choice [
      varDecl;
      whileStmt;
      forStmt;
      ifElseStmt;
      ifStmt;
      fnCallStmt;
      returnStmt;
      breakStmt;
    ]
  
let parse (source: string) : Result<Program, string> =
  match run program source with
  | Success (ast, _, _) -> Result.Ok ast
  | Failure (msg, _, _)  -> Result.Error msg