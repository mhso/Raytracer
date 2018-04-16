namespace Acceleration

module KD_tree = 

    //////////////////////////
    // TYPES AND EXCEPTIONS //
    //////////////////////////

    type coordinate = {x:float; y:float; z:float}

    exception KDException

    type Shape = S of float

    type BBox = { maxXYZ:coordinate; 
                  minXYZ:coordinate;
                  shape:Shape }
    
    type KDTree = Leaf of BBox list
                | Node of string * float * KDTree * KDTree
    
    type Ray = R of float * float * float


    ///////////////
    // FUNCTIONS //
    ///////////////

    val buildKDTree : boxes:list<BBox> -> KDTree

    val traverseKDTree : tree:KDTree -> ray:Ray -> BBox