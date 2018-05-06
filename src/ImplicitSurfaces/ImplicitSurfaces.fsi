namespace Tracer.ImplicitSurfaces

module Main =

  open Tracer.Basics

  type hf = Ray -> (float * Vector) option

  type baseShape = Tracer.BaseShape.BaseShape
  type shape = Tracer.Basics.Shape

  val mkImplicit : string -> baseShape

  // needed for testing
  type expr = ExprParse.expr
  val partialDerivative : string -> expr -> expr
  val substWithRayVars : expr -> expr
  val getVarMap : Ray -> Map<string, float>

  type Ray = Tracer.Basics.Ray
  type poly = ExprToPoly.poly
  type unipoly = PolyToUnipoly.unipoly

  val newtonRaph : unipoly -> float -> float option
