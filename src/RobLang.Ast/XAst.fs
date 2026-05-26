module RobLang.XAst

open RobLang.Ast

type NodeId = NodeId of System.Guid

let newNodeId () : NodeId = NodeId (System.Guid.NewGuid())

type XNode<'a> =
  { id      : NodeId
  ; scopeId : NodeId
  ; node    : 'a
  }

type XExpr    = XNode<Expr>
type XStmt    = XNode<Stmt>
type XProgram = XStmt list

let annotate (program : Program) : XProgram =
  let wrap (scopeId : NodeId) (node : 'a) : XNode<'a> =
    { id = newNodeId (); scopeId = scopeId; node = node }

  let rootScope = newNodeId ()
  List.map (wrap rootScope) program
