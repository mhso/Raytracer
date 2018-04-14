namespace Tracer.ImplicitSurfaces

module ExprToPoly =
  
  type expr = ExprParse.expr
  val subst : expr -> (string * expr) -> expr

  type simpleExpr
  val ppSimpleExpr : simpleExpr -> string
  val exprToSimpleExpr : expr -> simpleExpr

  type poly = P of Map<int,simpleExpr>
  val ppPoly : string -> poly -> string
  val simpleExprToPoly : simpleExpr -> string -> poly

  val exprToPoly : expr -> string -> poly
  val solveSimpleExpr : simpleExpr -> Map<string,float> -> float

  