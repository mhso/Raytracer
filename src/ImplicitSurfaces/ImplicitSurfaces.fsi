namespace Tracer.ImplicitSurfaces

module Main =

  open Tracer.Basics

  type hf = Ray -> (float * Vector * MatteMaterial) option

  type shape =
    abstract hf : hf

  type baseShape =
    abstract mkShape : Material -> shape

  val mkImplicit : string -> shape

  // needed for testing
  type expr = ExprParse.expr
  val partial : string -> expr -> expr
  val substWithRayVars : expr -> expr

  type Ray = Tracer.Basics.Ray
  type poly = ExprToPoly.poly

  val newtonRaph : poly -> Ray -> float -> float option
