namespace Tracer.ImplicitSurfaces

module Main =

  open Tracer.Basics

  type hf = Ray -> (float * Vector) option

  type shape =
    abstract hf : hf

  type baseShape =
    abstract mkShape : TexturedMaterial -> shape

  val mkImplicit : string -> baseShape