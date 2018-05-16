﻿namespace Tracer.Basics

module RegularGrids =

    // Used for debug, will print to console etc. 
    let debug = false
    let debugBuild = false
    let debugTraverse = false

    // Type of the BVHTree, with Nodes and Leafs.
    type RGStructure = Shape list[,,]*int*int*int*BBox  

    let clamp (x:float,b:int) : float =
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
        if boxes.Length = 0 then failwith "findOuterBoundingBoxLowHighPoints -> Empty array"
        let first = boxes.[0]
        let lowX = Array.fold (fun acc (box:BBox) -> if box.lowPoint.X < acc then box.lowPoint.X else acc) first.lowPoint.X boxes
        let lowY = Array.fold (fun acc (box:BBox) -> if box.lowPoint.Y < acc then box.lowPoint.Y else acc) first.lowPoint.Y boxes
        let lowZ = Array.fold (fun acc (box:BBox) -> if box.lowPoint.Z < acc then box.lowPoint.Z else acc) first.lowPoint.Z boxes
        let highX = Array.fold (fun acc (box:BBox) -> if box.highPoint.X > acc then box.highPoint.X else acc) first.highPoint.X boxes
        let highY = Array.fold (fun acc (box:BBox) -> if box.highPoint.Y > acc then box.highPoint.Y else acc) first.highPoint.Y boxes
        let highZ = Array.fold (fun acc (box:BBox) -> if box.highPoint.Z > acc then box.highPoint.Z else acc) first.highPoint.Z boxes
        
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
    let buildStructure (shapes:array<Shape>) : RGStructure =
        if shapes.Length = 0 then failwith "build-> Cannont build with empty shape array" // Shapes needed for build.

        let boxes = convertShapesToBBoxes shapes // Return bounding boxes from shapes.
        let lowPoint, highPoint = findOuterBoundingBoxLowHighPoints boxes // lo/high point of outer bounding box.
        if debugBuild then printfn "build->lowPoint, highPoint: %A %A " lowPoint highPoint
        let box = BBox (lowPoint, highPoint)

        if shapes.Length < 5 then
            let grid = Array3D.zeroCreate<Shape list> 1 1 1
            grid.[0,0,0] <- []
            for shape in shapes do 
                grid.[0,0,0] <- shape::grid.[0,0,0]
            (grid, 1, 1, 1, box)
        else
            
            let w = Vector(highPoint.X-lowPoint.X, highPoint.Y-lowPoint.Y, highPoint.Z-lowPoint.Z) // Vector from low to high of the outer bounding box.
            let m = 2.0 // m a constant to ajust the size of the grid structure.
            let n = shapes.Length // Number of shapes.
            let nx, ny, nz = calcAxisCells w.X w.Y w.Z m n
            if debugBuild then printfn "build->grid: %i %i %i " nx ny nz
            let bbx = calcBbox nx w.X
            let bby = calcBbox ny w.Y
            let bbz = calcBbox nz w.Z
            if debugBuild then printfn "build->bbx bby bbz: %f %f %f " bbx bby bbz

            let grid = Array3D.zeroCreate<Shape list> nx ny nz
        
            for iz in 0..nz-1 do
                    for iy in 0..ny-1 do
                        for ix in 0..nx-1 do
                            grid.[ix,iy,iz] <- []
 
            for shape in shapes do 
                let bb = shape.getBoundingBox()
                let ixMin = int(clamp((bb.lowPoint.X-lowPoint.X)*bbx, nx-1))
                let iyMin = int(clamp((bb.lowPoint.Y-lowPoint.Y)*bby, ny-1))
                let izMin = int(clamp((bb.lowPoint.Z-lowPoint.Z)*bbz, nz-1))

                let ixMax = int(clamp((bb.highPoint.X-lowPoint.X)*bbx, nx-1))
                let iyMax = int(clamp((bb.highPoint.Y-lowPoint.Y)*bby, ny-1))
                let izMax = int(clamp((bb.highPoint.Z-lowPoint.Z)*bbz, nz-1))
                if debugBuild then printfn "build->min: %i %i %i" ixMin iyMin izMin
                if debugBuild then printfn "build->max: %i %i %i" ixMax iyMax izMax

                for iz=izMin to izMax do
                    for iy=iyMin to iyMax do
                        for ix=ixMin to ixMax do
                            grid.[ix,iy,iz] <- shape::grid.[ix,iy,iz]
                            if debugBuild then printfn "Add shape to array at pos %i %i %i " ix iy iz
                            if debugBuild then printfn "Shape %A" shape
        
            if debugBuild then printfn "build->shapes.Length: %i" shapes.Length
            if debugBuild then printfn "build->Grid: %A" grid
            (grid, nx, ny, nz, box)
    
    let build (shapes:array<Shape>) : RGStructure = 
        let structure = buildStructure shapes
        structure
        
  // ######################### TRAVERSAL REGULAR GRID #########################
    let calcIxIyIz (p:Point) (bbox:BBox) (nx:int) (ny:int) (nz:int) : int*int*int = 
        let ix = clamp(((p.X-bbox.lowPoint.X) / (bbox.highPoint.X-bbox.lowPoint.X))*float nx, nx-1)
        let iy = clamp(((p.Y-bbox.lowPoint.Y) / (bbox.highPoint.Y-bbox.lowPoint.Y))*float ny, ny-1)
        let iz = clamp(((p.Z-bbox.lowPoint.Z) / (bbox.highPoint.Z-bbox.lowPoint.Z))*float nz, nz-1)
        (int ix, int iy, int iz)
    
    let calcNextStepStop (dA:float) (tA:float) (iA:int) (dtA:float) (nA:int) :float*int*int=
        let dAVal = dA.CompareTo 0.0
        match dAVal with
        | -1 -> tA+(float nA-float iA)*dtA, -1, -1
        | 0 -> infinity, -1, -1
        | 1 -> tA+(float iA+1.)*dtA, 1, nA
        | _ -> failwith "nextStepStop -> float compareTo out of range (-1,0,1)"

    // Functions finds closest hit of a ray in structure.
    let closestHit (shapeList:Shape list) (ray:Ray) : HitPoint option =
        match shapeList with
        | [] -> None
        | shapes ->
                                    let mutable closestHit = None
                                    let mutable closestDist = infinity
                                    for shape in shapes do
                                        let hit = shape.hitFunction ray
                                        let dist = hit.Time
                                        if hit.DidHit && dist < closestDist then
                                            closestDist <- dist
                                            closestHit <- Some hit
                                    if debug then printfn "closestHit -> Leaf found return hit at dist %f" closestDist
                                    closestHit
        | _ ->  if debug then printfn "closestHit -> None..."
                None

    //Function for search of the grid.
    let search (structure:RGStructure) (shapes:Shape array) (ray:Ray): HitPoint option =
        //printfn "search: %A" structure
        let grid, nx, ny, nz, bbox = structure
        match bbox.intersectRG ray with
        | Some (t,t',tx,ty,tz,tx',ty',tz') ->
                                                let p = ray.GetOrigin

                                                if not (bbox.isInside p) then
                                                    let d = ray.GetDirection
                                                    let p = p+(t*d)
                                                
                                                    let ix, iy, iz = calcIxIyIz p bbox nx ny nz
                                                    if debugTraverse then printfn "ix, iy, iz %i %i %i" ix iy iz

                                                    let dtx : float = (tx'-tx)/float nx
                                                    let dty : float = (ty'-ty)/float ny
                                                    let dtz : float = (tz'-tz)/float nz

                                                    let txNext, ixStep, ixStop = calcNextStepStop d.X tx ix dtx nx
                                                    let tyNext, iyStep, iyStop = calcNextStepStop d.Y ty iy dty ny
                                                    let tzNext, izStep, izStop = calcNextStepStop d.Z tz iz dtz nz

                                                    let rec loop ix iy iz txNext tyNext tzNext =
                                                        if debugTraverse then printfn "ix iy iz txNext tyNext tzNext %i %i %i %f %f %f" ix iy iz txNext tyNext tzNext
                                                        if debugTraverse then printfn "ixStep ixStop %i %i" ixStep ixStop
                                                        let checkForHit = closestHit grid.[ix,iy,iz] ray
                                                        if txNext<tyNext && txNext<tzNext then
                                                            if debugTraverse then printfn "search->Not inside->txNext<tyNext && txNext<tzNext"
                                                            match checkForHit with
                                                            | Some hitFound ->
                                                                if hitFound.Time<txNext then Some hitFound
                                                                else
                                                                    if (ix+ixStep) = ixStop then
                                                                        None
                                                                    else loop (ix+ixStep) iy iz (txNext+dtx) tyNext tzNext
                                                            | _ ->  if (ix+ixStep) = ixStop then
                                                                        None
                                                                    else loop (ix+ixStep) iy iz (txNext+dtx) tyNext tzNext
                                                        else
                                                            if tyNext<tzNext then
                                                                if debugTraverse then printfn "search->Not inside->tyNext<tzNext"
                                                                match checkForHit with
                                                                | Some hitFound ->
                                                                    if hitFound.Time<tyNext then Some hitFound
                                                                    else
                                                                        if (iy+iyStep) = iyStop then None
                                                                        else loop ix (iy+iyStep) iz txNext (tyNext+dty) tzNext
                                                                | _ -> if (iy+iyStep) = iyStop then None
                                                                        else loop ix (iy+iyStep) iz txNext (tyNext+dty) tzNext
                                                            else
                                                                if debugTraverse then printfn "search->Not inside->else"
                                                                match checkForHit with
                                                                | Some hitFound -> 
                                                                    if hitFound.Time<tzNext then Some hitFound
                                                                    else
                                                                        if (iz+izStep) = izStop then None
                                                                        else loop ix iy (iz+izStep) txNext tyNext (tzNext+dtz)
                                                                | _ ->  if (iz+izStep) = izStop then None
                                                                        else loop ix iy (iz+izStep) txNext tyNext (tzNext+dtz)
                                                    loop ix iy iz txNext tyNext tzNext
                                                else
                                                    if debugTraverse then printfn "search->Inside"
                                                    None                    
        | None -> None

    //Function for traversal of the structure.
    let traverse (structure:RGStructure) (ray:Ray) (shapes:array<Shape>) = 
        if debug then printfn "¤¤¤¤¤¤¤¤¤¤¤ structure BEGIN ¤¤¤¤¤¤¤¤¤¤¤"
        if debug then printfn "structure %A" structure
        if debug then printfn "¤¤¤¤¤¤¤¤¤¤¤ structure END ¤¤¤¤¤¤¤¤¤¤¤"
        match search structure shapes ray with
        | Some r -> r
        | None -> HitPoint ray