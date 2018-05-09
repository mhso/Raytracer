namespace Tracer

module RegularGrids =
    open Tracer.Basics

    // Used for debug, will print to console etc. 
    let debug = true

    let clamp (x:float,b:int) =
        match x with
        | x when x<0. -> 0.
        | x when x>float(b) -> float(b)
        | _ -> System.Math.Floor(x)

    let calcEdgeLength (wx:float) (wy:float) (wz:float) (n:float) : float = System.Math.Pow (((wx*wy*wz)/n),(1./3.))

    let calcAxisCell (m:float) (w:float) (s:float) : float = System.Math.Floor ((m*w)/s)+1.

    let calcAxisCells (wx:float) (wy:float) (wz:float) (m:float) (n:int) = 
        let s =  calcEdgeLength wx wy wz (float(n))
        let nx = int(calcAxisCell m wx s)
        let ny = int(calcAxisCell m wy s)
        let nz = int(calcAxisCell m wz s)
        (nx, ny, nz)

    // Function for getting combined outer low and high from a array og bounding boxes.
    let findOuterBoundingBoxLowHighPoints (boxes:array<BBox>) = 
        let lowX = Array.fold (fun acc (box:BBox) -> if box.lowPoint.X < acc then box.lowPoint.X else acc) infinity boxes
        let lowY = Array.fold (fun acc (box:BBox) -> if box.lowPoint.Y < acc then box.lowPoint.Y else acc) infinity boxes
        let lowZ = Array.fold (fun acc (box:BBox) -> if box.lowPoint.Z > acc then box.lowPoint.Z else acc) -infinity boxes
        let highX = Array.fold (fun acc (box:BBox) -> if box.highPoint.X > acc then box.highPoint.X else acc) -infinity boxes
        let highY = Array.fold (fun acc (box:BBox) -> if box.highPoint.Y > acc then box.highPoint.Y else acc) -infinity boxes
        let highZ = Array.fold (fun acc (box:BBox) -> if box.highPoint.Z < acc then box.highPoint.Z else acc) infinity boxes
        
        Point(lowX, lowY, lowZ), Point(highX, highY, highZ)
    
    // Function for converting a list of shapes to an array of their bounding boxes.
    let convertShapesToBBoxes (shapes:array<Shape>) : array<BBox> =
        let bboxArr : BBox[] = Array.zeroCreate (shapes.Length)
        for i in 0..bboxArr.Length-1 do
            bboxArr.[i] <- shapes.[i].getBoundingBox()
        bboxArr

 // ######################### BUILD REGULAR GRID #########################

    let calcBbox n w = float(n)/w
    
    // Function performs recursive searh in the grid, with a maximum distance from the ray origin.
    let build (shapes:array<Shape>) =

        let boxes = convertShapesToBBoxes shapes // Return bounding boxes from shapes.
        let lp, hp = findOuterBoundingBoxLowHighPoints boxes // lo/high point of outer bounding box.
        let w = Vector(hp.X-lp.Y, hp.Y-lp.Y, hp.Z-lp.Z) // Vector from low to high of the outer bounding box.
        let m = 2.0 // m a constant to ajust the size of the grid structure.
        let n = shapes.Length // Number of shapes.
        let nx, ny, nz = calcAxisCells w.X w.Y w.Z m n

        let gridArr = [for x in ]

        let bbx = calcBbox nx w.X
        let bby = calcBbox ny w.Y
        let bbz = calcBbox nz w.Z

        for shape in shapes do 
            let bb = shape.getBoundingBox()
            let ixMin = int(clamp((bb.lowPoint.X-lp.X)*bbx, nx-1))
            let iyMin = int(clamp((bb.lowPoint.Y-lp.Y)*bby, ny-1))
            let izMin = int(clamp((bb.lowPoint.Z-lp.Z)*bbz, nz-1))

            let ixMax = int(clamp((bb.highPoint.X-lp.X)*bbx, nx-1))
            let iyMax = int(clamp((bb.highPoint.Y-lp.Y)*bby, ny-1))
            let izMax = int(clamp((bb.highPoint.Z-lp.Z)*bbz, nz-1))

            for iz=izMin to izMax do
                for iy=iyMin to iyMax do
                    for ix=ixMin to ixMax do
                        printfn "Do stuff"
                
                
                
        
  // ######################### TRAVERSAL BVH TREE #########################
    // Function for traversal of the grid.
    //let traverse (treeNode:RGrind) (ray:Ray) (shapes:array<Shape>) =