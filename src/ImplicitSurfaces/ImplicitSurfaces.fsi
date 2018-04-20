namespace Tracer.ImplicitSurfaces

module Main =

  type Vector = Tracer.Basics.Vector
  type Point = Tracer.Basics.Point

  val mkImplicit : string -> (Point -> Vector -> (float * Vector))

