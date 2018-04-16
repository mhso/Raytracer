module Tracer.Sampling

val regular : int -> (float * float) []
val random : int -> int -> (float * float) []
val jittered : int -> int -> (float * float) []
val nRooks : int -> int -> (float * float) []
val multiJittered : int -> int -> (float * float) []
val mapToDisc : (float * float) [] -> (float * float) []
val mapToHemisphere : (float * float) [] -> (float * float * float) []