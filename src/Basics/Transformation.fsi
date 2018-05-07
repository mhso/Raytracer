module Tracer.Basics.Transformation

open Tracer.Basics
[<Sealed>]
type Matrix =
  static member multi : Matrix * Matrix -> Matrix
type Transformation
  
val mkMatrix : float array array -> Matrix
val getRowLength : Matrix -> int
val getColLength : Matrix -> int
val transpose : Matrix -> Matrix
val translate : x : float -> y : float -> z : float -> Transformation
val getList : Matrix -> float array array
val getMatrix : Transformation -> Matrix
val getInvMatrix : Transformation -> Matrix
val scale : width : float -> height : float -> depth : float -> Transformation
val vectorToMatrix : Vector -> Matrix
val pointToMatrix : Point -> Matrix
val matrixToVector : Matrix -> Vector
val matrixToPoint :  Matrix -> Point
val rotateX : angle : float -> Transformation
val rotateY : angle : float -> Transformation
val rotateZ : angle : float -> Transformation
val sheare : xy : float * xz :float * yx : float * yz : float * zx : float * zy : float-> Transformation
val mergeTransformations : Transformation list -> Transformation