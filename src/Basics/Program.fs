open Tracer.Basics
open Tracer.Basics.Sampling
open System
open Tracer.Basics.Render
open Tracer.Basics.Transform
//open System.Net.Mime.MediaTypeNames

[<EntryPoint>]
let main _ = 
    Sampling.main [|"regular"; "2"; "1"; "sphere"|] |> ignore
    0