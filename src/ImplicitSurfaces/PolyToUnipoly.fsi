namespace Tracer.ImplicitSurfaces

module PolyToUnipoly =

  type poly = ExprToPoly.poly
  type unipoly = 
    (int * float) list
    static member ( - ) : unipoly * unipoly -> unipoly
    static member ( % ) : unipoly * unipoly -> unipoly * unipoly
    static member ( * ) : unipoly * (int * float) -> unipoly

  val polyToUnipoly : poly -> float -> float -> float -> float -> float -> float -> unipoly
  val solveUnipoly : unipoly -> float -> float
  val unipolyDerivative : unipoly -> unipoly
  val sturmSeq : unipoly -> unipoly -> unipoly list
  val getInterval : unipoly list -> float -> float -> int -> (float * float * float) option
