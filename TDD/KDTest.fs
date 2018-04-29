module KDTest

open Acceleration.KD_tree
open Tracer.Basics
open Assert
let allTest = 
    printfn("KD Test")
    let KDBuild_Test = 
        let BBox1 = new ShapeBBox(Point(4.0, 4.0, 4.0),
                          Point(3.0, 3.0, 3.0),
                          shape = 1)
        let BBox2 = new ShapeBBox(Point(3.0, 3.0, 3.0),
                          Point(2.0, 2.0, 2.0),
                          shape = 2)
        let BBox3 = new ShapeBBox(Point(2.0, 2.0, 2.0),
                          Point(-1.0, -1.0, -1.0),
                          shape = 3)
        let BBox4 = new ShapeBBox(Point(1.0, 1.0, 1.0),
                          Point(0.0, 0.0, 0.0),
                          shape = 4)
        let BBox5 = new ShapeBBox(Point(0.0, 0.0, 0.0),
                          Point(-1.0, -1.0, -1.0),
                          shape = 5)
        let BBox6 = new ShapeBBox(Point(-4.0, -5.0, -5.0),
                          Point(-7.0, -7.0, -7.0),
                          shape = 6)
    
        let BBList = [BBox1;BBox2;BBox3;BBox4;BBox5;BBox6]
    
        let result = buildKDTree BBList

        let BBox11 = new ShapeBBox(Point(4.0, 4.0, 4.0),
                          Point(3.0, 3.0, 3.0),
                          shape = 1)
        let BBox22 = new ShapeBBox(Point(4.0, 4.0, 4.0),
                          Point(3.0, 3.0, 3.0),
                          shape = 1)

        Assert.Equal(BBox11, BBox22, "TestTest")

        let actual = 
            Node
              (0,1.0, Acceleration.KD_tree.BBox(Point(4.0, 4.0, 4.0), Point(-7.0, -7.0, -7.0)),
               Node
                 (0,2.0, Acceleration.KD_tree.BBox(Point(4.0, 4.0, 4.0), Point(-1.0, -1.0, -1.0)),
                  Node
                    (0,3.0, Acceleration.KD_tree.BBox(Point(4.0, 4.0, 4.0), Point(2.0, 2.0, 2.0)),
                     Leaf (Acceleration.KD_tree.BBox(Point(4.0, 4.0, 4.0), Point(3.0, 3.0, 3.0)), 
                          [Acceleration.KD_tree.ShapeBBox(Point(4.0, 4.0, 4.0),Point(3.0, 3.0, 3.0), 1)]),
                     Leaf (Acceleration.KD_tree.BBox(Point(3.0, 3.0, 3.0), Point(2.0, 2.0, 2.0)), 
                          [Acceleration.KD_tree.ShapeBBox(Point(3.0, 3.0, 3.0),Point(2.0, 2.0, 2.0), 2)])),
                  Leaf (Acceleration.KD_tree.BBox(Point(2.0, 2.0, 2.0), Point(-1.0, -1.0, -1.0)),
                       [Acceleration.KD_tree.ShapeBBox(Point(2.0, 2.0, 2.0),Point(-1.0, -1.0, -1.0), 3)])),
               Node
                 (0,0.0, Acceleration.KD_tree.BBox(Point(2.0, 2.0, 2.0), Point(-7.0, -7.0, -7.0)),
                  Leaf
                    (Acceleration.KD_tree.BBox(Point(2.0, 2.0, 2.0), Point(-1.0, -1.0, -1.0)),
                    [Acceleration.KD_tree.ShapeBBox(Point(2.0, 2.0, 2.0),Point(-1.0, -1.0, -1.0), 3); 
                     Acceleration.KD_tree.ShapeBBox(Point(1.0, 1.0, 1.0),Point(0.0, 0.0, 0.0), 4)]),
                  Node
                    (0,-4.0, Acceleration.KD_tree.BBox(Point(2.0, 2.0, 2.0), Point(-7.0, -7.0, -7.0)),
                     Leaf
                       (Acceleration.KD_tree.BBox(Point(2.0, 2.0, 2.0), Point(-1.0, -1.0, -1.0)),
                       [Acceleration.KD_tree.ShapeBBox(Point(0.0, 0.0, 0.0),Point(-1.0, -1.0, -1.0), 5); 
                        Acceleration.KD_tree.ShapeBBox(Point(2.0, 2.0, 2.0),Point(-1.0, -1.0, -1.0), 3)]),
                     Leaf (Acceleration.KD_tree.BBox(Point(-4.0, -4.0, -4.0), Point(-7.0, -7.0, -7.0)), 
                          [Acceleration.KD_tree.ShapeBBox(Point(-4.0, -4.0, -4.0),Point(-7.0, -7.0, -7.0), 6)]))))
        
        //Assert.Equal(actual, result, "KD-Build Test")

        let test1 = KDTree2(Acceleration.KD_tree.BBox(Point(1.0,1.0,1.0),Point(2.0,2.0,2.0)), 
                       [Acceleration.KD_tree.ShapeBBox(Point(1.0,1.0,1.0),Point(2.0,2.0,2.0),1);
                        Acceleration.KD_tree.ShapeBBox(Point(3.0,3.0,3.0),Point(2.0,2.0,2.0),1)])
        let test2 = KDTree2(Acceleration.KD_tree.BBox(Point(1.0,1.0,1.0),Point(2.0,2.0,2.0)), 
                       [Acceleration.KD_tree.ShapeBBox(Point(1.0,1.0,1.0),Point(2.0,2.0,2.0),1);
                        Acceleration.KD_tree.ShapeBBox(Point(3.0,3.0,3.0),Point(2.0,2.0,2.0),1)])

        printfn "%A" (test1.Equals(test2))
    KDBuild_Test