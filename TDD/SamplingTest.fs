﻿module SamplingTest

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

    let multiJittered_NRooksPropertyIsMaintained =
        setRandomSeed 42 // With this seed, nRooks property will be challenged.
        let sets = multiJittered 4 1
        let samples = sets.[0]
        let mutable result = true
        for i in 0..samples.Length-1 do
            if isRookThreatened samples i then result <- false
        Assert.True (result, "multiJittered_NRooksPropertyIsMaintained")

    let sampleSets_SetsAreShuffled = 
        setRandomSeed 19
        let sets1 = multiJittered 2 2
        let sample1_1 = Array.sort sets1.[0]
        let sample1_2 = Array.sort sets1.[1]
        
        System.Threading.Thread.Sleep(50) // Sleep so random changes (THIS SUCKS!!!)
        
        setRandomSeed 19
        let sets2 = multiJittered 2 2
        let sample2_1 = Array.sort sets2.[0]
        let sample2_2 = Array.sort sets2.[1]

        Assert.Equal (sample1_1, sample2_1, "sampleSets_SetsAreShuffled1")
        Assert.Equal (sample1_2, sample2_2, "sampleSets_SetsAreShuffled2")

    let sampleSets_SetsAreCorrectSize = 
        let sets = multiJittered 2 4
        Assert.Equal (4, sets.Length, "sampleSets_SetsAreCorrectSize")

    let sampleSets_SamplesAreShuffled =
        setRandomSeed 19
        let samples1 = (multiJittered 16 1).[0]

        System.Threading.Thread.Sleep(50) // Sleep so random changes (THIS SUCKS!!!)

        setRandomSeed 19
        let samples2 = (multiJittered 16 1).[0]

        Assert.True (not (samples1 = samples2), "sampleSets_SamplesAreShuffled1")
        let samples1 = Array.sort samples1
        let samples2 = Array.sort samples2
        Assert.Equal (samples1, samples2, "sampleSets_SamplesAreShuffled2")
    
    let sampleSets_SamplesAreCorrectSize =
        let samples = (multiJittered 4 1).[0]
        Assert.Equal (16, samples.Length, "sampleSets_SamplesAreCorrectSize")

    let mapToDisc_SamplesAreInCorrectQuadrants =
        let samples = (multiJittered 2 1).[0]
        let toDisc = Array.map mapToDisc samples
        let validArr = [|(true, true);(false, false);(true, false);(false, true)|]
        let result = Array.forall (fun (b1, b2) -> 
            Array.exists (fun (x, y) -> b1 = (x < 0.0) && b2 = (y < 0.0)) toDisc) validArr
        Assert.True (result, "mapToDisc_SamplesAreInCorrectQuadrants")

    let mapToDisc_SamplesAreInValidRange =
        let samples = (multiJittered 16 1).[0]
        let toDisc = Array.map mapToDisc samples
        let result = Array.forall (fun (x, y) -> 
                                            x > -0.9 && y > -0.9 || 
                                            x < 0.9 && y < 0.9 ||
                                            x < 0.9 && y > -0.9 ||
                                            x > -0.9 && y < 0.9) toDisc
        Assert.True (result, "mapToDisc_SamplesAreInValidRange")

    let Sampler_SamplerReturnsNextSample =
        let sampler = new Sampler(multiJittered, 8, 1)

        let next = sampler.Next()
        Assert.True (next > (0., 0.), "Sampler_SamplerReturnsNextSample")

    let Sampler_SamplerReturnsCurrentSample =
        let sampler = new Sampler(multiJittered, 8, 1)

        let next = sampler.Next()
        let current = sampler.Current
        Assert.Equal (next, current, "Sampler_SamplerReturnsCurrentSample")

    ()