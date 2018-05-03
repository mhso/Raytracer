module BVHTest

open Tracer.Basics
open Tracer.BVH
open Assert

let allTest = 

    let createShapeAndBBoxArr =
        let fig1 = SphereShape(Point(0.,0.,0.), 1., MatteMaterial(Colour.White))
        let fig2 = SphereShape(Point(5.,5.,5.), 2., MatteMaterial(Colour.Blue))
        let fig3 = SphereShape(Point(-3.,-3.,-3.), 4., MatteMaterial(Colour.Red))
        let fig4 = SphereShape(Point(7.,7.,7.), 1., MatteMaterial(Colour.Green))

        let shapeArr = [|(fig1:>Shape); (fig2:>Shape); (fig3:>Shape); (fig4:>Shape)|]
        let bboxArr : BBox[] = Array.zeroCreate (shapeArr.Length)
        //printfn "shapeArr.Length: %i" shapeArr.Length
        //printfn "bboxArr.Length: %i" bboxArr.Length
        for i in 0..shapeArr.Length-1 do
            bboxArr.[i] <- shapeArr.[i].getBoundingBox()
        //let bboxArr : BBox[] = [for i in 0..ShapeArr.Lenght do shapeArr.[i].getBoundingBox]
        shapeArr, bboxArr

    let bBox01 = BBox (Point(1., 0.6, -1.), Point(6., 9., -8.9))
    let bBox02 = BBox (Point(7., 3., -8.4), Point(12., 7., -16.6))
    let bBox03 = BBox (Point(8., 10., -8.9), Point(11.4, 13.5, -15.7))
    let bBox04 = BBox (Point(3.5, 10., -4.8), Point(5., 11.5, -7.3))
    let bBox05 = BBox (Point(3.0, y = 3.0, z = 3.0), Point(4.0, y = 4.0, z = 4.0))
    let bBox06 = BBox (Point(2.0, y = 2.0, z = 2.0), Point(3.0, y = 3.0, z = 3.0))
    let bBox07 = BBox (Point(-1.0, y = -1.0, z = -1.0), Point(2.0, y = 2.0, z = 2.0))
    let bBox08 = BBox (Point(1.0, y = 1.0, z = 1.0), Point(0.0, y = 0.0, z = 0.0))
    let bBox09 = BBox (Point(-1.0, y = -1.0, z = -1.0), Point(0.0, y = 0.0, z = 0.0))
    let bBox10 = BBox (Point(-7.0, y = -7.0, z = -7.0), Point(-4.0, y = -5.0, z = -5.0))

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
        let tree = buildBVHTree testBVHDataInputSmall
        //printfn "BVH Tree:\n %O" tree

        let expectedSmall = 
            (Node
                  (Node
                     (Leaf ([0],BBox (Point(1.,0.6,-1.), Point(6.,9.,-8.9))),Leaf ([1],BBox (Point(1.,0.6,-1.), Point(6.,9.,-8.9))),
                      BBox (Point(1.,0.6,-1.), Point(12.,9.,-16.6)),1),
                   Node
                     (Leaf ([2],BBox(Point(1.,0.6,-1.), Point(6.,9.,-8.9))),Leaf ([3],BBox(Point(1.,0.6,-1.), Point(6.,9.,-8.9))),
                      BBox(Point(1.,0.6,-1.), Point(12.,9.,-16.6)),1),BBox(Point(1.,0.6,-1.), Point(12.,13.5,-16.6)),1))

        //let expected3 = Node(Leaf([1], bBox01), Leaf([2], bBox01), bBox01, 99)

        Assert.Equal (expectedSmall,tree,"testBuildBVHTree")
    testBuildBVHTree

// ----------------------------- TEST BEGIN -----------------------------
    let testTraverse = 
        let ray = Ray(Point(-10.0,-10.0,-10.0), Vector(1.,1.,3.))
        let shapeArr, bboxArr = createShapeAndBBoxArr
        let tree = buildBVHTree (bboxArr)
        let result = traverse tree ray shapeArr infinity
        printfn "shapeArr: %A \n\n" shapeArr
        printfn "bboxArr: %A \n\n" bboxArr

        let expected = Some ((SphereShape(Point(0.,0.,0.), 1., MatteMaterial(Colour.White)):>Shape).hitFunction ray)
        Assert.Equal (expected,result,"testTraverse")
    testTraverse