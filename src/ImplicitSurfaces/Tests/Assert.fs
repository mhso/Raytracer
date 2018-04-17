namespace Tests

module Assert =

    type Assert() = 
        static member Equal (expected,result,name) = 
            if (expected = result) then printfn "--------Test Passed! %s" name
            else printfn "--------TEST FAILED!!! Expected (%O) Actual (%O) in %s" expected result name
        static member True (value,name) =
            if value then printfn "--------Test Passed! %s" name
            else printfn "--------TEST FAILED!!! Assert true is false (%O) in %s" value name