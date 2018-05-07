﻿namespace Tracer.ImplicitSurfaces

module PolyToUnipoly =

  type poly = ExprToPoly.poly
  type unipoly = 
    | UP of Map<int,float>
    static member ( - ) : unipoly * unipoly -> unipoly
    static member ( % ) : unipoly * unipoly -> unipoly * unipoly
    static member ( * ) : unipoly * (int * float) -> unipoly

  val polyToUnipoly : poly -> Map<string,float> -> unipoly
  val solveUnipoly : unipoly -> float -> float
  val unipolyDerivative : unipoly -> unipoly
  val sturmSeq : unipoly -> unipoly -> unipoly list
  val makeGuess : unipoly list -> float option
