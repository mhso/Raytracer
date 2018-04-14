namespace Tracer.ImplicitSurfaces

module PolyToHitFuntion =

  type Vector = Tracer.Basics.Vector
  type Point = Tracer.Basics.Point
  type poly = Tracer.ImplicitSurfaces.ExprToPoly.poly

  val getSecondDegreeHF : poly -> (Point -> Vector -> float * Vector)