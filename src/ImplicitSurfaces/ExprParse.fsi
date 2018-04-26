namespace Tracer.ImplicitSurfaces

module ExprParse =
  [<Sealed>]
     
  type terminal   
  exception ScanErrorException
  val scan : char seq -> terminal list
  val insertMult : terminal list -> terminal list

  type expr = 
    | FNum of float
    | FVar of string
    | FAdd of expr * expr
    | FMult of expr * expr
    | FDiv of expr * expr
    | FExponent of expr * int
    | FRoot of expr * int

  exception ParseErrorException
  val parse : terminal list -> expr
  val parseStr : char seq -> expr
  val dotAST : expr -> string

  val solveExpr : Map<string, float> -> expr -> float