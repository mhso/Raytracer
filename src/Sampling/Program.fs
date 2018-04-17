module Tracer.Sampling.Program

open System
open Tracer.Sampling.Sampling

[<EntryPoint>]
let main argsv =
    if Array.isEmpty argsv then 
        printfn "Error: No arguments given! Expected: [sampleMethod] [sampleAmount]."
        0
    else
    let method = argsv.[0]
    let amount = Int32.Parse(argsv.[1])
    let sets = if argsv.Length > 2 then Int32.Parse(argsv.[2]) else 1
    let fileName = "sampletest.png"
    let samples = 
        match method with
            | "regular" -> regular amount
            | "random"  -> (random amount sets).[0]
            | "jittered" -> (jittered amount sets).[0]
            | "nrooks"  -> (nRooks amount sets).[0]
            | "multi"   -> (multiJittered amount sets).[0]
            | _ -> regular 4
    if argsv.Length > 3 then
        if argsv.[3] = "disc" then drawDiscSamples (mapToDisc samples) fileName
        else if argsv.[3] = "sphere" then
            let e = if argsv.Length = 5 then float (Int32.Parse argsv.[4]) else 0.0
            drawSphereSamples (mapToHemisphere samples e) fileName true
    else drawSamples samples method fileName
    0