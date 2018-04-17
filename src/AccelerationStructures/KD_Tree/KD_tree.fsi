namespace Acceleration

module KD_tree = 

    //////////////////////////
    // TYPES AND EXCEPTIONS //
    //////////////////////////

    [<Sealed>]
    type coordinate

    exception KDException

    [<Sealed>]
    type Shape

    [<Sealed>]
    type BBox
    
    [<Sealed>]
    type KDTree
    
    [<Sealed>]
    type Ray


    ///////////////
    // FUNCTIONS //
    ///////////////

    val buildKDTree : boxes:list<BBox> -> KDTree

    val traverseKDTree : tree:KDTree -> ray:Ray -> BBox