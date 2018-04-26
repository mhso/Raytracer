// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// Learn more about F# at http://fsharp.org
open Assert

[<EntryPoint>]
let main argv =
    let before = new System.TimeSpan()
    printfn("-=-=-=-=-=-=- BVH Test -=-=-=-=-=-=-")
    BVHTest.allTest
    printfn("-=-=-=-=-=-=- Transformation Test -=-=-=-=-=-=-")
    TransformationTest.allTest
    printfn("-=-=-=-=-=-=- Shapes Test -=-=-=-=-=-=-")
    ShapeTest.allTest
    printfn ""
    printfn "-=-=-=-=-=-=- Sampling Test -=-=-=-=-=-=-"
    SamplingTest.allTest

    printfn ""
    printfn "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    printfn ""

    let passed = new System.TimeSpan() - before
    printfn "%d/%d Tests passed. Duration: %d ms." 
        Assert.AmountPassed (Assert.AmountFailed+Assert.AmountPassed) passed.Milliseconds
    printfn("PRESS A KEY TO EXIT")
    System.Console.ReadKey() |> ignore
    0