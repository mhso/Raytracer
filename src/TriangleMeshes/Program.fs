// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FParsec

[<EntryPoint>]
let main argv = 
    let answer = PLYParser.parsePLY @"D:\Users\Mathias\Documents\GIT-ITU\SecondYearProject\raytracer\resources\ply\bunny.ply"
    //printfn "%A" answer
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
