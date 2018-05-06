namespace Tracer.ImplicitSurfaces

module PolyToUnipoly =

  type poly = ExprToPoly.poly
  type unipoly = 
    | UP of Map<int,float>
    static member ( % ) : unipoly * unipoly -> unipoly

  val polyToUnipoly : poly -> Map<string,float> -> unipoly
  val solveUnipoly : unipoly -> float -> float
  val unipolyDerivative : unipoly -> unipoly