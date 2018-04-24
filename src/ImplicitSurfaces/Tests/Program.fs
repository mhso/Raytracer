[<EntryPoint>]
let main argv =
    Tests.ExprParseTests.allTest
    Tests.ExprToPolyTests.allTests
    Tests.ExprToPolyTests2.allTests
    Tests.ExprToPolyTests3.allTests
    printfn("PRESS A KEY TO EXIT")
    System.Console.ReadKey() |> ignore
    0

