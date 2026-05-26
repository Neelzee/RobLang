module RobLang.Ast

type Prefix =
  | Neg
  | Not

type BinOp =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or

type Expr =
  | Null
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Array of Expr list
  | Range of Expr * Expr * Expr option
  | Var of string
  | Op of Op
  | Call of string * Expr list
  | RobCall of string * Expr list
  | RobGet  of string

and Affix =
  | Index of Expr
  | Slice of Expr option * Expr option * Expr option

and Op =
  | Prefix of Prefix * Expr
  | Affix of Expr * Affix
  | Infix of Expr * BinOp * Expr

type Param = string * Expr option

type Stmt =
  | VarDecl of string * Expr
  | VarAssDecl of string * Expr
  | If of Expr * Block
  | While of Expr * Block
  | IfElse of Expr * Block * Block
  | For of string * Expr * Block
  | FnDecl of string * Param list * Block
  | FnCall of string * Expr list
  | Break
  | Return of Expr option
  | RobCallStmt of string * Expr list
  | RobAssStmt of string * Expr

and Block = Stmt list

type Program = Stmt list
