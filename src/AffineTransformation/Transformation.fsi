module Transformation
open Tracer.Basics

[<Sealed>]
type Transformation =
  static member multi : Transformation * Transformation -> Transformation
  
val mkTransformation : float list list -> Transformation
val getRowLength : Transformation -> int
val getColLength : Transformation -> int
val transpose : Transformation -> Transformation
val translate : x : float -> y : float -> z : float -> Transformation
val translateInv : x : float -> y : float -> z : float -> Transformation
val getList : Transformation -> float list list
val scale : width : float -> height : float -> depth : float -> Transformation
val scaleInv : width : float -> height : float -> depth : float -> Transformation
val rotateX : angle : float -> Transformation
val rotateY : angle : float -> Transformation
val rotateZ : angle : float -> Transformation
val rotateXInv : angle : float -> Transformation
val rotateYInv : angle : float -> Transformation
val rotateZInv : angle : float -> Transformation
val sheareXY : distance : float -> Transformation
val sheareXZ : distance : float -> Transformation
val sheareYX : distance : float -> Transformation
val sheareYZ : distance : float -> Transformation
val sheareZX : distance : float -> Transformation
val sheareZY : distance : float -> Transformation
//val sheareXYInv : distance : float -> Transformation
val mergeTransformations : Transformation list -> Transformation
//val transform : Shape -> Transformation -> Shape
val transformLight : Light -> Transformation -> Light