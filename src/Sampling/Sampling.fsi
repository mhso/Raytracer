module Tracer.Sampling.Sampling

val setRandomSeed : int -> unit
val regular : int -> (float * float) [][]
val random : int -> int -> (float * float) [][]
val jittered : int -> int -> (float * float) [][]
val nRooks : int -> int -> (float * float) [][]
val multiJittered : int -> int -> (float * float) [][]
val mapToDisc : (float * float) -> (float * float)
val mapToHemisphere : (float * float) -> float -> (float * float * float)

type Sampler =
    new: samplingAlgorighm:(int -> int -> (float * float) [][]) * sampleCount:int * sampleSetCount:int -> Sampler
    member Current: float * float
    member Next: unit -> float * float
    member SampleCount: int