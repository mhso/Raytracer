namespace Tracer.ImplicitSurfaces

module ExprToPoly =
  
  type expr = ExprParse.expr
  
  val subst : expr -> (string * expr) -> expr

  type atom = ANum of float | AExponent of string * int | ARadical of simpleExpr * int
  and atomGroup = atom list  
  and simpleExpr = SE of atomGroup list

  val ppSimpleExpr : simpleExpr -> string
  val exprToSimpleExpr : expr -> simpleExpr
  val simplifySimpleExpr : simpleExpr -> simpleExpr

  type poly = 
    | P of Map<int,simpleExpr>
    static member ( % ) : poly * poly -> poly
    
  val exprToPoly : expr -> string -> poly
  val ppPoly : string -> poly -> string
  val simpleExprToPoly : simpleExpr -> string -> poly

  val solveSE : Map<string,float> -> simpleExpr -> float
  val ppExpr : expr -> string
  val rewriteExpr : expr -> atom list list
  val simplifyAtomGroup : seq<atom> -> atomGroup
  val combine : atomGroup list -> atom list list -> atom list list

  val reducePolyConstants : poly -> Map<string,float> -> Map<int,float>
  val solveReducedPolyList : float -> (int * float) list -> float
  val polyDerivative : poly -> poly