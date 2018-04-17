[<EntryPoint>]
let main argv =
    Tests.ExprParseTests.allTest
    printfn("PRESS A KEY TO EXIT")
    System.Console.ReadKey() |> ignore
    0

