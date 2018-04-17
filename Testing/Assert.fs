module Assert

type assertion() = 
    member this.equal (expected,result) = 
    if (expected = result) then printfn("--------Test Passed!")
    else failwith("--------TEST FAILED!!!")