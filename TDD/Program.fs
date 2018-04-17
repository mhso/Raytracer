// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// Learn more about F# at http://fsharp.org

[<EntryPoint>]
let main argv =
<<<<<<< HEAD
    // TransformationTest.allTest
    BVHTest.allTest
=======
    printfn("-=-=-=-=-=-=- Transformation Test -=-=-=-=-=-=-")
    TransformationTest.allTest
    printfn "-=-=-=-=-=-=- Sampling Test -=-=-=-=-=-=-"
    SamplingTest.allTest
>>>>>>> 83f27261400e6d832fdb3a110526dd9b76c7fb96
    printfn("PRESS A KEY TO EXIT")
    System.Console.ReadKey() |> ignore
    0

