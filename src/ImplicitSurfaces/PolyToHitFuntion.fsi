namespace Tracer.ImplicitSurfaces

module PolyToHitFuntion =

  type Vector = Tracer.Vector
  type Point = Tracer.Point
  type poly = Tracer.ImplicitSurfaces.ExprToPoly.poly

  val getSecondDegreeHF : poly -> (Point -> Vector -> float * Vector)


