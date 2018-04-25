module BVHTest

open Tracer.Basics
open Tracer.BVH
open Assert
let allTest = 

    let bBox01 = {  lowXYZ = Point(1., 0.6, -1.);
                    highXYZ = Point(6., 9., -8.9);
                    }
    let bBox02 = {  lowXYZ = Point(7., 3., -8.4);
                    highXYZ = Point(12., 7., -16.6);
                    }
    let bBox03 = {  lowXYZ = Point(8., 10., -8.9);
                    highXYZ = Point(11.4, 13.5, -15.7);
                    }
    let bBox04 = {  lowXYZ = Point(3.5, 10., -4.8);
                    highXYZ = Point(5., 11.5, -7.3);
                    }
    let bBox05 = {  lowXYZ = Point(3.0, y = 3.0, z = 3.0);
                    highXYZ = Point(4.0, y = 4.0, z = 4.0);
                    }
    let bBox06 = {  lowXYZ = Point(2.0, y = 2.0, z = 2.0);
                    highXYZ = Point(3.0, y = 3.0, z = 3.0);
                    }
    let bBox07 = {  
                    lowXYZ = Point(-1.0, y = -1.0, z = -1.0);
                    highXYZ = Point(2.0, y = 2.0, z = 2.0);
                    }
    let bBox08 = {  highXYZ = Point(1.0, y = 1.0, z = 1.0);
                    lowXYZ = Point(0.0, y = 0.0, z = 0.0);
                    }
    let bBox09 = {  lowXYZ = Point(-1.0, y = -1.0, z = -1.0);
                    highXYZ = Point(0.0, y = 0.0, z = 0.0);
                    }
    let bBox10 = {  lowXYZ = Point(-7.0, y = -7.0, z = -7.0);
                    highXYZ = Point(-4.0, y = -5.0, z = -5.0);
                    }

    let testBBoxDataInput = List.toArray [bBox01; bBox02; bBox03]
    let testBVHDataInputSmall = List.toArray [bBox01; bBox02; bBox03; bBox04]
    let testBVHDataInputLarge = List.toArray [bBox05; bBox06; bBox07; bBox08; bBox09; bBox10]

    // ----------------------------- TEST BEGIN -----------------------------
    let testSortListByAxisX = 
        let indexList = [0;1;2]
        let expected = [0;1;2]
        let result = sortListByAxis indexList testBBoxDataInput 0
        Assert.Equal (expected,result,"testSortListByAxisX")
    testSortListByAxisX

    // ----------------------------- TEST BEGIN -----------------------------
    let testSortListByAxisY = 
        let indexList = [0;1;2]
        let expected = [0;1;2]
        let result = sortListByAxis indexList testBBoxDataInput 1
        Assert.Equal (expected,result,"testSortListByAxisY")
    testSortListByAxisY

    // ----------------------------- TEST BEGIN -----------------------------
    let testSortListByAxisZ =
        let indexList = [0;1;2]
        let expected = [2;1;0]
        let result = sortListByAxis indexList testBBoxDataInput 2
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

    let testBuildBVHTree = 
        let tree = buildBVHTree (testBVHDataInputSmall)
        // trees
        //printfn "BVH Tree:\n %O" tree
        //let expected = 
        //        (Node
        //                (Node
        //                    (Leaf [5],Node (Leaf [4],Leaf [2],{lowXYZ = Point(2.,2.,3.);
        //                                                    highXYZ = Point(4.,4.,3.);},1),
        //                    {lowXYZ = Point(-1.,-1.,3.);
        //                    highXYZ = Point(4.,4.,2.);},1),
        //                Node
        //                    (Leaf [3],Node (Leaf [1],Leaf [0],{lowXYZ = Point(2.,2.,3.);
        //                                                    highXYZ = Point(4.,4.,3.);},1),
        //                    {lowXYZ = Point(-1.,-1.,3.);
        //                    highXYZ = Point(4.,4.,2.);},1),{lowXYZ = Point(-7.,-7.,3.);
        //                                            highXYZ = Point(4.,4.,-5.);},1))
        //let expected2 = 
        //                (Node
        //                    (Leaf [0],Node (Leaf [1],Leaf [2],{lowXYZ = Point(1.,0.6,-1.); highXYZ = Point(12.,9.,-16.6);},1),
        //                    {lowXYZ = Point(1.,0.6,-1.);
        //                    highXYZ = Point(12.,13.5,-16.6);},1))

        //let expectedSmall = 
        //        (Node
        //            (Node
        //            (Leaf [5],Node (Leaf [4],Leaf [2],{lowXYZ = Point(2.,2.,3.); highXYZ = Point(4.,4.,3.);},1),
        //                {lowXYZ = Point(-1.,-1.,3.);
        //                highXYZ = Point(4.,4.,2.);},1),
        //                Node
        //                    (Leaf [3],
        //                        Node (Leaf [1],Leaf [0],{lowXYZ = Point(2.,2.,3.); highXYZ = Point(4.,4.,3.);},1),
        //                    {lowXYZ = Point(-1.,-1.,3.); highXYZ = Point(4.,4.,2.);},1),
        //                    {
        //                        lowXYZ = Point(-7.,-7.,3.);
        //                        highXYZ = Point(4.,4.,-5.);},1))

        let expected3 = Node(Leaf([1], bBox01), Leaf([2], bBox01), bBox01, 99)

        Assert.Equal (expected3,tree,"testBuildBVHTree")
    testBuildBVHTree