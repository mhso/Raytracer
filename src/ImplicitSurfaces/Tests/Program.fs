[<EntryPoint>]
let main argv =
    Tests.ExprParseTests.allTest
    Tests.ExprToPolyTests.allTests
    printfn("PRESS A KEY TO EXIT")
    System.Console.ReadKey() |> ignore
    0

