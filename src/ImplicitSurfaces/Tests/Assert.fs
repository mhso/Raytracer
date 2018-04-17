namespace Tests

module Assert =

    type Assert() = 

        static member EqualMany testlist f =
            for (name, input, expected) in testlist do
                Assert.Equal (f input) expected name

        static member Equal actual expected name = 
            if (actual = expected) 
                then printfn "--------Test Passed! %s" name
            else printfn "--------TEST FAILED! %s \n ******** \nExpected: (%O) \nActual: (%O) \n ********" name expected actual

        static member True value name =
            if value 
                then printfn "--------Test Passed! %s" name
            else printfn "--------TEST FAILED!!! Assert true is false (%O) in %s" value name

        static member Fail name input f =
            try
                let res = f input
                printfn "--------TEST FAILED! %s \n ******** \nValue: %O\nOutput: %O\n********" name input res 
            with
              | _ -> printfn "--------Test Passed! %s" name
