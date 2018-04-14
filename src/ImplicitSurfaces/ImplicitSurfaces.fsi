namespace Tracer

module ImplicitSurfaces =

  type Vector = Tracer.Basics.Vector
  type Point = Tracer.Basics.Point

  // here I am assuming that the shape is centered at 0.0 0.0 0.0??
  val mkImplicit : string -> (Point -> Vector -> (float * Vector))

