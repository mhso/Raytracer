module KDTest

open Acceleration.KD_tree
open Assert
let allTest = 
    printfn("Transformation Test")
    let BBox1 = {maxXYZ = {x = 1.0; y = 2.0; z = 3.0};
                 minXYZ = {x = -1.0; y = -2.0; z = -3.0};
                 shape = S(1.0)}
    let BBox2 = {maxXYZ = {x = -1.0; y = -2.0; z = -3.0};
             minXYZ = {x = -4.0; y = -5.0; z = -5.0};
             shape = S(1.0)}
    let BBox3 = {maxXYZ = {x = -4.0; y = -5.0; z = -5.0};
             minXYZ = {x = -7.0; y = -8.0; z = -9.0};
             shape = S(1.0)}
    let BBox4 = {maxXYZ = {x = 4.0; y = 4.0; z = 6.0};
             minXYZ = {x = 2.0; y = 3.0; z = 4.0};
             shape = S(1.0)}
    
    let BBList = [BBox1;BBox2;BBox3;BBox4]
    
    let testGetRowLengthWith3x5Returns3 = 
        buildKDTree BBList

        
        Assert.Equal (3,result,"getRowLengthWith3x5")
    testGetRowLengthWith3x5Returns3