// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Acceleration
[<EntryPoint>]
let main argv = 
    let result = KD_tree.buildKDTree (KD_tree.BBList1)
    printfn "%A" result
    0 // return an integer exit code
