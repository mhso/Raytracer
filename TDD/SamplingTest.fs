module SamplingTest

open Tracer.Sampling.Sampling
open System
open Assert

// These two helper-methods are copied from Sampling.fs (which is duplication, sadly),
// but I did not want to expose these methods in the original signature file.
let getGrid (v:float) max = int(v*(float max))

let illegalSpots = [|(2, 1);(-2, 1);(2, -1);(-2, -1);(1, 2);(1, -2);(-1, 2);(-1, -2)|]

let isRookThreatened (samples:(float*float) []) i =
    let n = samples.Length
    let valX, valY = samples.[i]
    let gridX, gridY = getGrid valX n, getGrid valY n
    let mutable result = true
    for (x,y) in samples do
        if not 
            (Array.exists  (fun (x2, y2) -> (gridX+x2) = (getGrid x n) && 
                                            (gridY+y2) = (getGrid y n)) illegalSpots)
        then result <- false
    result

// Returns actual jittered array vs. expected array
let getJitteredEvaluation (samples:(float*float) []) =
    let n = int(Math.Sqrt (float samples.Length))
    let gridExpected = [|for x in 0..n-1 do for y in 0..n-1 do yield (x, y)|]
    let gridValues = [|for (x, y) in samples do yield (getGrid x n, getGrid y n)|]
    let gridExpected = Array.sort gridExpected
    let gridValues = Array.sort gridValues
    (gridExpected, gridValues)

let allTest =
    let jittered_JitteredPropertyIsMaintained =
        let sets = jittered 4 1
        let expected, actual = getJitteredEvaluation sets.[0]
        Assert.Equal (expected, actual, "jittered_JitteredPropertyIsMaintained")

    let nRooks_NRooksPropertyIsMaintained =
        setRandomSeed 42 // With this seed, nRooks property will be challenged.
        let sets = nRooks 8 1
        let samples = sets.[0]
        let mutable result = true
        for i in 0..samples.Length-1 do
            if isRookThreatened samples i then result <- false
        Assert.True (result, "nRooks_NRooksPropertyIsMaintained")

    let multiJittered_JitteredPropertyIsMaintained =
        let sets = multiJittered 4 1
        let expected, actual = getJitteredEvaluation sets.[0]
        Assert.Equal (expected, actual, "multiJittered_JitteredPropertyIsMaintained")

    jittered_JitteredPropertyIsMaintained
    nRooks_NRooksPropertyIsMaintained
    