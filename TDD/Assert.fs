module Assert

type Assert() = 
    static member Equal (expected,result,name) = 
        if (expected = result) then printfn "--------Test Passed! %s" name
        else printfn "--------TEST FAILED!!! Expected (%A) Actual (%A) in %s" expected result name
    static member True (value,name) =
        if value then printfn "--------Test Passed! %s" name
        else printfn "--------TEST FAILED!!! Assert true is false (%A) in %s" value name