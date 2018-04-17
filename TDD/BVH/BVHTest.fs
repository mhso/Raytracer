module BVHTest

open Tracer.BVH
open Assert
let allTest = 
    printfn("BVH Test")
    // ----------------------------- qsort TEST BEGIN -----------------------------
    let box1Test = {  lowXYZ = {x=1.; y=0.6; z=(-1.)};
                      highXYZ = {x=6.5; y=9.; z=(-8.9)};
                      shape = S(5.0) }
    let box2Test = {  lowXYZ = {x=7.; y=3.; z=(-8.4)};
                      highXYZ = {x=12.; y=7.; z=(-16.6)};
                      shape = S(4.0) }
    let box3Test = {  lowXYZ = {x=8.; y=10.; z=(-8.9)};
                      highXYZ = {x=11.4; y=13.5; z=(-15.7)};
                      shape = S(3.0) }

    let qsortTestDataInput = [box1Test; box2Test; box3Test]

    //let qsortTestX = qsort qsortTestDataInput 0
    //let qsortTestY = qsort qsortTestDataInput 1
    //let qsortTestZ = qsort qsortTestDataInput 2

    let testSortListByAxis = 
        let expected = List.empty
        let result = sortListByAxis qsortTestDataInput 0
        Assert.Equal (expected,result,"testSortListByAxis")
    testSortListByAxis
    // ----------------------------- qsort TEST END -----------------------------