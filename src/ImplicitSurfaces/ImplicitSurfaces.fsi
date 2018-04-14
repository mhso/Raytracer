namespace Tracer

module ImplicitSurfaces =

  type Vector = Tracer.Vector
  type Point = Tracer.Point

  // here I am assuming that the shape is centered at 0.0 0.0 0.0??
  val mkImplicit : string -> (Point -> Vector -> (float * Vector))

