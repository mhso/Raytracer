module KDTest

open Acceleration.KD_tree
open Tracer.Basics
open Assert
let allTest = 
    printfn("KD Test")
    let KDBuild_Test = 
        let BBox1 = ShapeBBox(Point(4.0, 4.0, 4.0),
                          Point(3.0, 3.0, 3.0),
                          shape = 1)
        let BBox2 = ShapeBBox(Point(3.0, 3.0, 3.0),
                          Point(2.0, 2.0, 2.0),
                          shape = 2)
        let BBox3 = ShapeBBox(Point(2.0, 2.0, 2.0),
                          Point(-1.0, -1.0, -1.0),
                          shape = 3)
        let BBox4 = ShapeBBox(Point(1.0, 1.0, 1.0),
                          Point(0.0, 0.0, 0.0),
                          shape = 4)
        let BBox5 = ShapeBBox(Point(0.0, 0.0, 0.0),
                          Point(-1.0, -1.0, -1.0),
                          shape = 5)
        let BBox6 = ShapeBBox(Point(-4.0, -5.0, -5.0),
                          Point(-7.0, -7.0, -7.0),
                          shape = 6)
    
        let BBList1 = [BBox1;BBox2;BBox3;BBox4;BBox5;BBox6]
    
    
        let result = buildKDTree BBList1

        
        printfn "%A" result
        Assert.Equal (result,result,"KD-Build")
    KDBuild_Test