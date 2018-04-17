module BVHTest

open Tracer.Basics
open Tracer.BVH
open Assert
let allTest = 
    printfn("BVH Test")

    let bBox01 = {  lowXYZ = {x=1.; y=0.6; z=(-1.)};
                      highXYZ = {x=6.5; y=9.; z=(-8.9)};
                      shape = S(5.0) }
    let bBox02 = {  lowXYZ = {x=7.; y=3.; z=(-8.4)};
                      highXYZ = {x=12.; y=7.; z=(-16.6)};
                      shape = S(4.0) }
    let bBox03 = {  lowXYZ = {x=8.; y=10.; z=(-8.9)};
                      highXYZ = {x=11.4; y=13.5; z=(-15.7)};
                      shape = S(3.0) }

    let testBBoxDataInput = [bBox01; bBox02; bBox03]

    // ----------------------------- TEST BEGIN -----------------------------
    let testSortListByAxisX = 
        let expected = [bBox01; bBox02; bBox03]
        let result = sortListByAxis testBBoxDataInput 0
        Assert.Equal (expected,result,"testSortListByAxisX")
    testSortListByAxisX

    // ----------------------------- TEST BEGIN -----------------------------
    let testSortListByAxisY = 
        let expected = [bBox01; bBox02; bBox03]
        let result = sortListByAxis testBBoxDataInput 1
        Assert.Equal (expected,result,"testSortListByAxisY")
    testSortListByAxisY

    // ----------------------------- TEST BEGIN -----------------------------
    let testSortListByAxisZ = 
        let expected = [bBox03; bBox02; bBox01]
        let result = sortListByAxis testBBoxDataInput 2
        Assert.Equal (expected,result,"testSortListByAxisZ")
    testSortListByAxisZ

// ----------------------------- TEST BEGIN -----------------------------
    let testFindOuterBoundingBox = 
        let expected = (Point(1.,0.6,-1.), Point(12.,13.5,-16.6))
        let result = findOuterBoundingBox testBBoxDataInput
        Assert.Equal (expected,result,"testOuterBoundingBox")
    testFindOuterBoundingBox

    //let testBuildBVHTree = 
    //    let expected = List.empty
    //    let result = buildBVHTree sortTestDataInput
    //    Assert.Equal (expected,result,"testgetOuterBoundingBox")
    //testBuildBVHTree

// ----------------------------- getOuterBoundinBox TEST END -----------------------------