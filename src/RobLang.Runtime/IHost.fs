module RobLang.Runtime.IHost

open RobLang.Ast

type IHost =
  abstract member Call  : string -> Expr list -> Result<Expr, string>
  abstract member Get   : string -> Result<Expr, string>
  abstract member Set   : string -> Expr -> Result<unit, string>
  abstract member Print : string -> Result<unit, string>
