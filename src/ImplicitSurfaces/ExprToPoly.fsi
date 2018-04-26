namespace Tracer.ImplicitSurfaces

module ExprToPoly =
  
  type expr = ExprParse.expr
  
  val subst : expr -> (string * expr) -> expr

  type atom = ANum of float | AExponent of string * int | ARadical of simpleExpr * int
  and atomGroup = atom list  
  and simpleExpr = SE of atomGroup list

  val ppSimpleExpr : simpleExpr -> string
  val exprToSimpleExpr : expr -> simpleExpr

  type poly = P of Map<int,simpleExpr>
  val exprToPoly : expr -> string -> poly
  val ppPoly : string -> poly -> string
  val simpleExprToPoly : simpleExpr -> string -> poly

  val solveSimpleExpr : Map<string,float> -> simpleExpr -> float  