open Tracer.Basics
open Tracer.Basics.Textures
open Tracer.Basics.Sampling
open Tracer.BaseShape
open Tracer.Basics.Render
open Tracer.Basics.Transform
open System
open System.Diagnostics
open Tracer.Basics.Transformation

[<EntryPoint>]
let main _ = 
    let sampler = multiJittered 32 127
    let mutable res = 0.0
    let timer = Stopwatch.StartNew()
    for i in 0..1920 do
        for j in 0..1080 do
            let set = sampler.NextSet()
            for (x, y) in set do
                res <- res + x + y
    timer.Stop()
    printfn "New: %d ms" timer.ElapsedMilliseconds
    printfn "%f" res

    let sampler = multiJittered 32 127
    let mutable res = 0.0
    let timer = Stopwatch.StartNew()
    for i in 0..1920 do
        for j in 0..1080 do
            for k in 0..sampler.SampleCount-1 do
                let (x, y) = sampler.Next()
                res <- res + x + y
    timer.Stop()
    printfn "Old: %d ms" timer.ElapsedMilliseconds
    printfn "%f" res
    Console.ReadKey() |> ignore
    0