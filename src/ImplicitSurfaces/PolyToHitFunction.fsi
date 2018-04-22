namespace Tracer.ImplicitSurfaces

module PolyToHitFunction =

  type Vector = Tracer.Basics.Vector
  type Point = Tracer.Basics.Point
  type poly = Tracer.ImplicitSurfaces.ExprToPoly.poly

  val getSecondDegreeHF : poly -> (Point -> Vector -> float * Vector)


