// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FParsec

[<EntryPoint>]
let main argv = 
    let result = PLYParser.parse PLYParser.stringToFloatListList 
        @"-0.0378297 0.12794 0.00447467 0.850855 0.5 
        -0.0447794 0.128887 0.00190497 0.900159 0.5 
        -0.0680095 0.151244 0.0371953 0.398443 0.5 
        -0.00228741 0.13015 0.0232201 0.85268 0.5 
        -0.0226054 0.126675 0.00715587 0.675938 0.5 
        -0.0251078 0.125921 0.00624226 0.711533 0.5 
        -0.0371209 0.127449 0.0017956 0.888639 0.5 
        0.033213 0.112692 0.0276861 0.652757 0.5 
        0.0380425 0.109755 0.0161689 0.708171 0.5"
    printfn "%A" result
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
