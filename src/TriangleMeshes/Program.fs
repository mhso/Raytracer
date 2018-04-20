// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FParsec

[<EntryPoint>]
let main argv = 
    let result = PLYParser.parse PLYParser.stringToFloatList "-0.0378297 0.12794 0.00447467 0.850855 0.5 "
    printfn "%A" result
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
