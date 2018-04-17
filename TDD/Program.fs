﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// Learn more about F# at http://fsharp.org

[<EntryPoint>]
let main argv =
    printfn("-=-=-=-=-=-=- BVH Test -=-=-=-=-=-=-")
    BVHTest.allTest
    printfn("-=-=-=-=-=-=- Transformation Test -=-=-=-=-=-=-")
    TransformationTest.allTest
    printfn ""
    printfn "-=-=-=-=-=-=- Sampling Test -=-=-=-=-=-=-"
    SamplingTest.allTest
    printfn("PRESS A KEY TO EXIT")
    System.Console.ReadKey() |> ignore
    0

