module RobLang.Ast

type Prefix =
  | Neg

type BinOp =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Gt
  | And
  | Or
  | Mod

type Expr =
  | Null
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Array of Expr list
  | Var of string
  | Op of Op
  | Call of string * Expr list

and Affix =
  | Index of Expr

and Op =
  | Prefix of Prefix * Expr
  | Affix of Expr * Affix 
  | Infix of Expr * BinOp * Expr

type Param = string * Expr option

type Stmt =
  | VarDecl of string * Expr
  | If of Expr * Block
  | While of Expr * Block
  | IfElse of Expr * Block * Block
  | For of string * Expr * Block
  | FnDecl of string * Param list * Block
  | FnCall of string * Expr list
  | Break
  | Return of Expr option

and Block = Stmt list

type Program = Stmt list