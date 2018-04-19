module BVHTest

open Tracer.Basics
open Tracer.BVH
open Assert
let allTest = 

    let bBox01 = {  lowXYZ = {x=1.; y=0.6; z=(-1.)};
                    highXYZ = {x=6.5; y=9.; z=(-8.9)};
                    shape = S(1.0) }
    let bBox02 = {  lowXYZ = {x=7.; y=3.; z=(-8.4)};
                    highXYZ = {x=12.; y=7.; z=(-16.6)};
                    shape = S(2.0) }
    let bBox03 = {  lowXYZ = {x=8.; y=10.; z=(-8.9)};
                    highXYZ = {x=11.4; y=13.5; z=(-15.7)};
                    shape = S(3.0) }
    let bBox04 = {  lowXYZ = {x = 3.0; y = 3.0; z = 3.0};
                    highXYZ = {x = 4.0; y = 4.0; z = 4.0};
                    shape = S(4.0)}
    let bBox05 = {  lowXYZ = {x = 2.0; y = 2.0; z = 2.0};
                    highXYZ = {x = 3.0; y = 3.0; z = 3.0};
                    shape = S(5.0)}
    let bBox06 = {  
                    lowXYZ = {x = -1.0; y = -1.0; z = -1.0};
                    highXYZ = {x = 2.0; y = 2.0; z = 2.0};
                    shape = S(6.0)}
    let bBox07 = {  highXYZ = {x = 1.0; y = 1.0; z = 1.0};
                    lowXYZ = {x = 0.0; y = 0.0; z = 0.0};
                    shape = S(7.0)}
    let bBox08 = {  lowXYZ = {x = -1.0; y = -1.0; z = -1.0};
                    highXYZ = {x = 0.0; y = 0.0; z = 0.0};
                    shape = S(8.0)}
    let bBox09 = {  lowXYZ = {x = -7.0; y = -7.0; z = -7.0};
                    highXYZ = {x = -4.0; y = -5.0; z = -5.0};
                    shape = S(9.0)}

    //let testBBoxDataInput = [bBox01; bBox02; bBox03]
    let testBBoxDataInput = [bBox01; bBox02; bBox03; bBox04; bBox05; bBox06]

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
    let testFindOuterBoundingBoxLowHighPoints = 
        let expected = (Point(1.,0.6,-1.), Point(12.,13.5,-16.6))
        let result = findOuterBoundingBoxLowHighPoints testBBoxDataInput
        Assert.Equal (expected,result,"testFindOuterBoundingBoxLowHighPoints")
    testFindOuterBoundingBoxLowHighPoints

// ----------------------------- TEST BEGIN -----------------------------
    let testFindLargestBoundingBoxSideLengths = 
        let outer = findOuterBoundingBoxLowHighPoints testBBoxDataInput
        let expected = (1, 12.9)
        let axis, lenght = findLargestBoundingBoxSideLengths outer
        Assert.Equal (expected,(axis, lenght),"testFindLargestBoundingBoxSideLengths")
    testFindLargestBoundingBoxSideLengths

// ----------------------------- TEST BEGIN -----------------------------

    //let bBList1 = [bBox01; bBox02; bBox03; bBox04; bBox05; bBox06]

    let testBuildBVHTree = 
        let tree = buildBVHTree testBBoxDataInput
        tree
        //Assert.Equal (expected,(axis, lenght),"testFindLargestBoundingBoxSideLengths")
    testBuildBVHTree