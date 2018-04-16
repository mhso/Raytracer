module GeneralTest

    let AssertEqual expected result = 
        if (expected = result) then printfn("Test Passed!")
        else failwith("TEST FAILED!!!")


    
