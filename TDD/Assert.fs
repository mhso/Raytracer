module Assert

type Assert() = 
    static member Equal (expected,result,name) = 
        if (expected = result) then printfn "--------Test Passed! %s" name
        else printfn "--------TEST FAILED!!! Expected (%O) Actual (%O) in %s" expected result name