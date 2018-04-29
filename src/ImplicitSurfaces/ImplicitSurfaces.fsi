namespace Tracer.ImplicitSurfaces

module Main =

  open Tracer.Basics

  type expr = ExprParse.expr
  val partial : string -> expr -> expr
  val substWithRayVars : expr -> expr

  type hf = Ray -> (float * Vector * MatteMaterial) option

  type shape =
    abstract hf : hf

  (*type baseShape =
    abstract mkShape : TexturedMaterial -> shape*)

  val mkImplicit : string -> shape