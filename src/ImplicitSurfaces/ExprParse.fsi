namespace Tracer.ImplicitSurfaces

module ExprParse =
  [<Sealed>]
  
  type terminal
    
  exception ScanErrorException
  val scan: char seq -> terminal list
  val insertMult: terminal list -> terminal list

  type expr
  exception ParseErrorException
  val parse: terminal list -> expr
  val parseStr: char seq -> expr
  val dotAST: expr -> string
