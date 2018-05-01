// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FParsec

[<EntryPoint>]
let main argv = 
    let answer = PLYParser.parsePLY @"..\..\..\..\resources\ply\head1.ply"

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
