module SamplingTest

open Tracer.Sampling.Sampling
open System
open Assert

let allTest =
    let jittered_JitteredPropertyIsMaintained =
        setRandomSeed(42)
        let sets = multiJittered 4 1
        let samples = sets.[0]
        let n = int(Math.Sqrt (float samples.Length))
        let gridExpected = [|for x in 0..n-1 do for y in 0..n-1 do yield (x, y)|]
        let gridValues = [|for (x, y) in samples do yield (getGrid x n, getGrid y n)|]
        let gridValues = 
            Array.sortWith (fun (x1, y1) (x2, y2) -> 
                if x1 > x2 && y1 > y2 then 1 else if x1 = x2 && y1 = y2 then 0 else -1) gridValues
        Array.forall2 (fun g1 g2 -> g1 = g2) gridExpected gridValues
    jittered_JitteredPropertyIsMaintained