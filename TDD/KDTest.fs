module KDTest

open Acceleration.KD_tree
open Assert
let allTest = 
    printfn("KD Test")
    let KDBuild_Test = 
        let BBox1 = {maxXYZ = {x = 4.0; y = 4.0; z = 4.0};
                     minXYZ = {x = 3.0; y = 3.0; z = 3.0};
                     shape = S(1.0)}
        let BBox2 = {maxXYZ = {x = 3.0; y = 3.0; z = 3.0};
                     minXYZ = {x = 2.0; y = 2.0; z = 2.0};
                     shape = S(1.0)}
        let BBox3 = {maxXYZ = {x = 2.0; y = 2.0; z = 2.0};
                     minXYZ = {x = -1.0; y = -1.0; z = -1.0};
                     shape = S(1.0)}
        let BBox4 = {maxXYZ = {x = 1.0; y = 1.0; z = 1.0};
                     minXYZ = {x = 0.0; y = 0.0; z = 0.0};
                     shape = S(1.0)}
        let BBox5 = {maxXYZ = {x = 0.0; y = 0.0; z = 0.0};
                     minXYZ = {x = -1.0; y = -1.0; z = -1.0};
                     shape = S(1.0)}
        let BBox6 = {maxXYZ = {x = -4.0; y = -5.0; z = -5.0};
                     minXYZ = {x = -7.0; y = -7.0; z = -7.0};
                     shape = S(1.0)}
    
        let BBList = [BBox1;BBox2;BBox3;BBox4;BBox5;BBox6]
    
    
        let result = buildKDTree BBList

        let actual =
         Node
          ("x",1.0,{x = 4.0;
                  y = 4.0;
                  z = 4.0;},{x = -7.0;
                             y = -7.0;
                             z = -7.0;},
          Node
           ("x",2.0,{x = 4.0;
                     y = 4.0;
                     z = 4.0;},{x = -1.0;
                                y = -1.0;
                                z = -1.0;},
            Node
              ("x",3.0,{x = 4.0;
                        y = 4.0;
                        z = 4.0;},{x = 2.0;
                                   y = 2.0;
                                   z = 2.0;},
               Leaf ({x = 4.0;
                      y = 4.0;
                      z = 4.0;},{x = 3.0;
                                 y = 3.0;
                                 z = 3.0;},[{maxXYZ = {x = 4.0;
                                                       y = 4.0;
                                                       z = 4.0;};
                                             minXYZ = {x = 3.0;
                                                       y = 3.0;
                                                       z = 3.0;};
                                             shape = S 1.0;}]),
               Leaf ({x = 3.0;
                      y = 3.0;
                      z = 3.0;},{x = 2.0;
                                 y = 2.0;
                                 z = 2.0;},[{maxXYZ = {x = 3.0;
                                                       y = 3.0;
                                                       z = 3.0;};
                                             minXYZ = {x = 2.0;
                                                       y = 2.0;
                                                       z = 2.0;};
                                             shape = S 1.0;}])),
            Leaf ({x = 2.0;
                   y = 2.0;
                   z = 2.0;},{x = -1.0;
                              y = -1.0;
                              z = -1.0;},[{maxXYZ = {x = 2.0;
                                                     y = 2.0;
                                                     z = 2.0;};
                                           minXYZ = {x = -1.0;
                                                     y = -1.0;
                                                     z = -1.0;};
                                           shape = S 1.0;}])),
          Node
           ("x",0.0,{x = 2.0;
                     y = 2.0;
                     z = 2.0;},{x = -7.0;
                                y = -7.0;
                                z = -7.0;},
            Leaf ({x = 2.0;
                   y = 2.0;
                   z = 2.0;},{x = -1.0;
                              y = -1.0;
                              z = -1.0;},[{maxXYZ = {x = 1.0;
                                                     y = 1.0;
                                                     z = 1.0;};
                                           minXYZ = {x = 0.0;
                                                     y = 0.0;
                                                     z = 0.0;};
                                           shape = S 1.0;}; {maxXYZ = {x = 2.0;
                                                                       y = 2.0;
                                                                       z = 2.0;};
                                                             minXYZ = {x = -1.0;
                                                                       y = -1.0;
                                                                       z = -1.0;};
                                                             shape = S 1.0;}]),
            Node
              ("x",-4.0,{x = 2.0;
                         y = 2.0;
                         z = 2.0;},{x = -7.0;
                                    y = -7.0;
                                    z = -7.0;},
               Leaf
                 ({x = 2.0;
                   y = 2.0;
                   z = 2.0;},{x = -1.0;
                              y = -1.0;
                              z = -1.0;},[{maxXYZ = {x = 0.0;
                                                     y = 0.0;
                                                     z = 0.0;};
                                           minXYZ = {x = -1.0;
                                                     y = -1.0;
                                                     z = -1.0;};
                                           shape = S 1.0;}; {maxXYZ = {x = 2.0;
                                                                       y = 2.0;
                                                                       z = 2.0;};
                                                             minXYZ = {x = -1.0;
                                                                       y = -1.0;
                                                                       z = -1.0;};
                                                             shape = S 1.0;}]),
               Leaf ({x = -4.0;
                      y = -4.0;
                      z = -4.0;},{x = -7.0;
                                  y = -7.0;
                                  z = -7.0;},[{maxXYZ = {x = -4.0;
                                                         y = -5.0;
                                                         z = -5.0;};
                                               minXYZ = {x = -7.0;
                                                         y = -7.0;
                                                         z = -7.0;};
                                               shape = S 1.0;}]))))
    
        Assert.Equal (actual,result,"KD-Build")
    KDBuild_Test