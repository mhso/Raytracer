﻿module BVHTest

open Tracer.Basics
open Tracer.BVH
open Assert

let allTest = 
    // Used for debug, will print to console etc. 
    let debug = true
    0

//    // ----------------------------- Common data -----------------------------
//    let createShapeAndBBoxArr (shapeArr: Shape array) =
//        let bboxArr : BBox[] = Array.zeroCreate (shapeArr.Length)
//        for i in 0..shapeArr.Length-1 do
//            bboxArr.[i] <- shapeArr.[i].getBoundingBox()
//        shapeArr, bboxArr
    
//    // Figures/Shapes
//    let fig1 = SphereShape(Point(0.,0.,0.), 1., Textures.mkMatTexture(MatteMaterial(Colour.White, 1., Colour.White, 1.)))
//    let fig2 = SphereShape(Point(5.,5.,5.), 2., Textures.mkMatTexture(MatteMaterial(Colour.Blue, 1., Colour.Blue, 1.)))
//    let fig3 = SphereShape(Point(-3.,-3.,-3.), 4., Textures.mkMatTexture(MatteMaterial(Colour.Red, 1., Colour.Red, 1.)))
//    let fig4 = SphereShape(Point(7.,7.,7.), 1., Textures.mkMatTexture(MatteMaterial(Colour.Green, 1., Colour.Green, 1.)))

//    let boxTexture = Textures.mkMatTexture(MatteMaterial(Colour.Red, 1., Colour.Red, 1.))
//    let fig5 = Box(Point(1.0,0.6,1.0), Point(6.5,9.0,8.7), boxTexture, boxTexture, boxTexture, boxTexture, boxTexture, boxTexture)
//    let fig6 = Box(Point(8.0,10.0,8.7), Point(11.4,13.5,15.7), boxTexture, boxTexture, boxTexture, boxTexture, boxTexture, boxTexture)
//    let fig7 = Box(Point(7.0,3.0,8.4), Point(12.0,7.0,16.5), boxTexture, boxTexture, boxTexture, boxTexture, boxTexture, boxTexture)
//    let fig8 = Box(Point(3.5,10.0,4.8), Point(5.0,11.5,7.3), boxTexture, boxTexture, boxTexture, boxTexture, boxTexture, boxTexture)

//    // Bounding boxes
//    let bBox01 = BBox (Point(1., 0.6, -1.), Point(6., 9., -8.9))
//    let bBox02 = BBox (Point(7., 3., -8.4), Point(12., 7., -16.6))
//    let bBox03 = BBox (Point(8., 10., -8.9), Point(11.4, 13.5, -15.7))
//    let bBox04 = BBox (Point(3.5, 10., -4.8), Point(5., 11.5, -7.3))
//    let bBox05 = BBox (Point(3.0, y = 3.0, z = 3.0), Point(4.0, y = 4.0, z = 4.0))
//    let bBox06 = BBox (Point(2.0, y = 2.0, z = 2.0), Point(3.0, y = 3.0, z = 3.0))
//    let bBox07 = BBox (Point(-1.0, y = -1.0, z = -1.0), Point(2.0, y = 2.0, z = 2.0))
//    let bBox08 = BBox (Point(1.0, y = 1.0, z = 1.0), Point(0.0, y = 0.0, z = 0.0))
//    let bBox09 = BBox (Point(-1.0, y = -1.0, z = -1.0), Point(0.0, y = 0.0, z = 0.0))
//    let bBox10 = BBox (Point(-7.0, y = -7.0, z = -7.0), Point(-4.0, y = -5.0, z = -5.0))

//    let testBBoxDataInput = List.toArray [bBox01; bBox02; bBox03]
//    let testBVHDataInputSmall = List.toArray [bBox01; bBox02; bBox03; bBox04]
//    let testBVHDataInputLarge = List.toArray [bBox05; bBox06; bBox07; bBox08; bBox09; bBox10]

//    // ----------------------------- TEST BEGIN -----------------------------
//    let testSortListByAxisX = 
//        let indexList = [0;1;2]
//        let expected = [0;1;2]
//        let result = sortListByAxis indexList testBBoxDataInput 0
//        Assert.Equal (expected,result,"testSortListByAxisX")
//    testSortListByAxisX

//    // ----------------------------- TEST BEGIN -----------------------------
//    let testSortListByAxisY = 
//        let indexList = [0;1;2]
//        let expected = [0;1;2]
//        let result = sortListByAxis indexList testBBoxDataInput 1
//        Assert.Equal (expected,result,"testSortListByAxisY")
//    testSortListByAxisY

//    // ----------------------------- TEST BEGIN -----------------------------
//    let testSortListByAxisZ =
//        let indexList = [0;1;2]
//        let expected = [2;1;0]
//        let result = sortListByAxis indexList testBBoxDataInput 2
//        Assert.Equal (expected,result,"testSortListByAxisZ")
//    testSortListByAxisZ

//// ----------------------------- TEST BEGIN -----------------------------
//    let testFindOuterBoundingBoxLowHighPoints = 
//        let expected = (Point(1.,0.6,-1.), Point(12.,13.5,-16.6))
//        let result = findOuterBoundingBoxLowHighPoints testBBoxDataInput
//        Assert.Equal (expected,result,"testFindOuterBoundingBoxLowHighPoints")
//    testFindOuterBoundingBoxLowHighPoints

//// ----------------------------- TEST BEGIN -----------------------------
//    let testFindLargestBoundingBoxSideLengths = 
//        let outer = findOuterBoundingBoxLowHighPoints testBBoxDataInput
//        let expected = (1, 12.9)
//        let axis, lenght = findLargestBoundingBoxSideLengths outer
//        Assert.Equal (expected,(axis, lenght),"testFindLargestBoundingBoxSideLengths")
//    testFindLargestBoundingBoxSideLengths

//// ----------------------------- TEST BEGIN -----------------------------
//    let testBuildBVHTree = 
//        let shapeArr = [|(fig1:>Shape); (fig2:>Shape); (fig3:>Shape); (fig4:>Shape)|]
//        let shapeArr, bboxArr = createShapeAndBBoxArr shapeArr
//        let tree = build shapeArr

//        let expected =
//                (Node
//                  (Node
//                     (Leaf
//                        ([2],
//                         BBox(Point(-1.000001,-1.000001,-1.000001), Point(1.000001,1.000001,1.000001))),
//                      Leaf
//                        ([0],
//                         BBox(Point(-1.000001,-1.000001,-1.000001), Point(1.000001,1.000001,1.000001))),
//                      BBox(Point(-1.000001,-1.000001,2.999999), Point(7.000001,7.000001,1.000001)),
//                      1),
//                   Node
//                     (Leaf
//                        ([1],
//                         BBox(Point(-1.000001,-1.000001,-1.000001), Point(1.000001,1.000001,1.000001))),
//                      Leaf
//                        ([3],
//                         BBox(Point(-1.000001,-1.000001,-1.000001), Point(1.000001,1.000001,1.000001))),
//                      BBox(Point(-1.000001,-1.000001,2.999999), Point(7.000001,7.000001,1.000001)),
//                      1),
//                   BBox(Point(-7.000001,-7.000001,5.999999), Point(8.000001,8.000001,1.000001)),
//                   1))

//        Assert.Equal (expected,tree,"testBuildBVHTree")
//    testBuildBVHTree

//// ----------------------------- TEST BEGIN -----------------------------
//    let testTraverse = 
//        let ray = Ray(Point(3.0,4.0,3.0), Vector(1.,1.,-1.))
//        let shapeBoxArr = [|(fig5:>Shape); (fig6:>Shape); (fig7:>Shape); (fig8:>Shape)|]
//        let shapeArr, bboxArr = createShapeAndBBoxArr shapeBoxArr
//        let tree = build (shapeArr)
//        let result = traverse tree ray shapeArr
//        if debug then printfn "shapeArr: %A \n\n" shapeArr
//        if debug then printfn "bboxArr: %A \n\n" bboxArr
//        Assert.True (result.DidHit,"testTraverse")
//    testTraverse

//    //BBox Intersect test
//    //let bBox = BBox(Point(0.999999,0.599999,7.300001), Point(12.000001,13.500001,8.699999))
//    //let rayInside = Ray(Point(3.,4.,3.), Vector(0.577350269189626,0.577350269189626,-0.577350269189626))
//    //let hitInside = bBox.intersect(rayInside)
//    //Assert.True(hitInside.IsSome, "BVH -> test on BBox intersect, for point inside BBox")
//    //let rayOutside = Ray(Point(1.5,1.5,1.5), Vector(-0.5,-0.5,-0.5))
//    //let hitOutside = bBox.intersect(rayOutside)
//    //Assert.True(hitOutside.IsSome, "BVH -> test on BBox intersect, for point outside BBox")
//    //let rayOutsideMiss = Ray(Point(1.5,1.5,1.5), Vector(0.5,0.5,0.5))
//    //let hitOutsideMiss = bBox.intersect(rayOutsideMiss)
//    //Assert.True(hitOutsideMiss.IsNone, "BVH -> test on BBox not intersect, for point outside BBox")