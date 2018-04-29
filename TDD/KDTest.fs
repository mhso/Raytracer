module KDTest

open Acceleration.KD_tree
open Tracer.Basics
open Assert
let allTest = 
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
        let BBox6 = new ShapeBBox(Point(-4.0, -4.0, -4.0),
                          Point(-7.0, -7.0, -7.0),
                          shape = 6)
    
        let BBList = [BBox1;BBox2;BBox3;BBox4;BBox5;BBox6]
    
        let result = buildKDTree BBList

        let actual = 
            new KDTree
              (0,1.0, BBox(Point(4.0, 4.0, 4.0), Point(-7.0, -7.0, -7.0)),
               new KDTree
                 (0,2.0, BBox(Point(4.0, 4.0, 4.0), Point(-1.0, -1.0, -1.0)),
                  new KDTree
                    (0,3.0, BBox(Point(4.0, 4.0, 4.0), Point(2.0, 2.0, 2.0)),
                     new KDTree 
                          (BBox(Point(4.0, 4.0, 4.0), Point(3.0, 3.0, 3.0)), 
                          [Acceleration.KD_tree.ShapeBBox(Point(4.0, 4.0, 4.0),Point(3.0, 3.0, 3.0), 1)]),
                     new KDTree 
                          (BBox(Point(3.0, 3.0, 3.0), Point(2.0, 2.0, 2.0)), 
                          [Acceleration.KD_tree.ShapeBBox(Point(3.0, 3.0, 3.0),Point(2.0, 2.0, 2.0), 2)])),
                  new KDTree 
                       (BBox(Point(2.0, 2.0, 2.0), Point(-1.0, -1.0, -1.0)),
                       [Acceleration.KD_tree.ShapeBBox(Point(2.0, 2.0, 2.0),Point(-1.0, -1.0, -1.0), 3)])),
               new KDTree
                 (0,0.0, BBox(Point(2.0, 2.0, 2.0), Point(-7.0, -7.0, -7.0)),
                  new KDTree
                    (BBox(Point(2.0, 2.0, 2.0), Point(-1.0, -1.0, -1.0)),
                    [Acceleration.KD_tree.ShapeBBox(Point(1.0, 1.0, 1.0),Point(0.0, 0.0, 0.0), 4);
                     Acceleration.KD_tree.ShapeBBox(Point(2.0, 2.0, 2.0),Point(-1.0, -1.0, -1.0), 3)]),
                  new KDTree
                    (0,-4.0, BBox(Point(2.0, 2.0, 2.0), Point(-7.0, -7.0, -7.0)),
                     new KDTree
                       (BBox(Point(2.0, 2.0, 2.0), Point(-1.0, -1.0, -1.0)),
                       [Acceleration.KD_tree.ShapeBBox(Point(0.0, 0.0, 0.0),Point(-1.0, -1.0, -1.0), 5); 
                        Acceleration.KD_tree.ShapeBBox(Point(2.0, 2.0, 2.0),Point(-1.0, -1.0, -1.0), 3)]),
                     new KDTree 
                          (BBox(Point(-4.0, -4.0, -4.0), Point(-7.0, -7.0, -7.0)), 
                          [Acceleration.KD_tree.ShapeBBox(Point(-4.0, -4.0, -4.0),Point(-7.0, -7.0, -7.0), 6)]))))

        let BBox11 = new ShapeBBox(Point(4.0, 4.0, 4.0),
                          Point(3.0, 3.0, 3.0),
                          shape = 1)
        let BBox22 = new ShapeBBox(Point(4.0, 4.0, 4.0),
                          Point(3.0, 3.0, 3.0),
                          shape = 1)


        Assert.Equal(BBox11, BBox22, "KD Test Box Match")
        Assert.Equal(actual, result, "KD-Build Test")
    KDBuild_Test