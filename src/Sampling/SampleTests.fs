module Tracer.Sampling.Tests
open Tracer.Sampling
open System

let jittered_JitteredPropertyIsMaintained =
    rand <- new Random(42)
    let samples = (multiJittered 4 1).[0]
    let n = int(Math.Sqrt (float samples.Length))
    let gridExpected = [|for x in 0..n-1 do for y in 0..n-1 do yield (x, y)|]
    let gridValues = [|for (x, y) in samples do yield (getGrid x n, getGrid y n)|]
    let gridValues = 
        Array.sortWith (fun (x1, y1) (x2, y2) -> 
            if x1 > x2 && y1 > y2 then 1 else if x1 = x2 && y1 = y2 then 0 else -1) gridValues
    Array.forall2 (fun g1 g2 -> g1 = g2) gridExpected gridValues

let nRooks_NRooksPropertyIsMaintained =
    false

let multiJittered_JitteredPropertyIsMaintained =
    false

let multiJittered_NRooksPropertyIsMaintained =
    false

let sampleSets_SetIsCorrectSize =
    false

let sampleSets_SamplePointsAreRandomized =
    false

let sampleSets_SampleSetsAreRandomized =
    false

let sampleSets_FullSamplingWith127SetsBelowBenchmark =
    false

let mapToDisc_SingleSampleIsMappedCorrectly =
    false

let mapToHemisphere_SingleSampleIsMappedCorrectly =
    false