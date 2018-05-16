namespace Tracer.Basics

module BVH = 
    
    // Used for debug, will print to console etc. 
    let debugBuild          = false
    let debugBuildCounts    = true
    let debugTravers        = false
    let debugSort           = false

    // Type of the BVHTree, with Nodes and Leafs.
    type BVHStructure = | Leaf of List<int>*BBox
                        | Node of BVHStructure*BVHStructure*BBox*int

    // Function for sorting a int list, based on array of bounding boxes and axis value.
    ////let rec sortListByAxis (indexList:list<int>) (boxes:array<BBox>) (axis:int) =
    ////  match indexList with
    ////  | [] -> []
    ////  | x :: xs ->
    ////      let less, great = 
    ////          match axis with
    ////          // Sort by x axis
    ////          | 0 -> let filterLess = fun e -> boxes.[e].lowPoint.X <= boxes.[x].lowPoint.X
    ////                 let filterGreat = fun e -> boxes.[e].lowPoint.X >  boxes.[x].lowPoint.X
    ////                 filterLess, filterGreat
    ////          // Sort by y axis
    ////          | 1 -> let filterLess = fun e -> boxes.[e].lowPoint.Y <= boxes.[x].lowPoint.Y
    ////                 let filterGreat = fun e -> boxes.[e].lowPoint.Y >  boxes.[x].lowPoint.Y
    ////                 filterLess, filterGreat
    ////          // Sort by z axis
    ////          | _ -> let filterLess = fun e -> boxes.[e].lowPoint.Z <= boxes.[x].lowPoint.Z
    ////                 let filterGreat = fun e -> boxes.[e].lowPoint.Z >  boxes.[x].lowPoint.Z
    ////                 filterLess, filterGreat

    ////      let lesser    = sortListByAxis (xs |> List.filter(less)) (boxes) axis
    ////      let greater   = sortListByAxis (xs |> List.filter(great)) (boxes) axis
    ////      lesser @ [x] @ greater
    
    // Function for sorting a int list, based on array of bounding boxes and axis value.
    let rec sortListByAxis (indexList:list<int>) (boxes:array<BBox>) (axis:int) =
        let sort ax = 
                match ax with
                | 0 -> (fun c -> boxes.[c].lowPoint.X)
                | 1 -> (fun c -> boxes.[c].lowPoint.Y)
                | 2 -> (fun c -> boxes.[c].lowPoint.Z)
        List.sortBy (sort axis) indexList

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
    
    // Function for finding the longest axis lenght of a cube
    let findLargestBoundingBoxSideLengths (box:(Point*Point)) : int*float =
        let lowPoint, highPoint = box
        let x, y, z = (highPoint.X - lowPoint.X), (highPoint.Y - lowPoint.Y), (highPoint.Z - lowPoint.Z)
        let mutable value = (0, 0.)

        if x > 0. then value <- (0, x)
        if y > 0. then value <- (1, y)
        if y < z then value <- (2, z)
        value
    
    // Function to find the min-values from a bounding box based on the axis value.
    let findAxisMinMaxValues (box:BBox) axis =
        let lowPoint, highPoint = box.lowPoint, box.highPoint;
        match axis with
        | 0 -> (lowPoint.X, highPoint.X)
        | 1 -> (lowPoint.Y, highPoint.Y)
        | 2 -> (lowPoint.Z, highPoint.Z)
        | _ -> invalidArg "findAxisMinMaxValues invalid axis value" "Axis value needs to be between 0-2."
    
    // Converts from int index to box array
    let rec getBoxArrFromIndexes (indexes:list<int>) (boxes:array<BBox>) : array<BBox> =
        [|for i in 0..(indexes.Length-1) -> boxes.[indexes.[i]]|]
    
    // Function for converting a list of shapes to an array of their bounding boxes.
    let convertShapesToBBoxes (shapes:array<Shape>) : array<BBox> =
        let bboxArr : BBox[] = Array.zeroCreate (shapes.Length)
        for i in 0..bboxArr.Length-1 do
            bboxArr.[i] <- shapes.[i].getBoundingBox()
        bboxArr


 // ######################### BUILD BVH TREE #########################
    let mutable totalNodes = 0
    let mutable totalLeafs = 0
    // Build BVH structure from a list of shapes.
    let buildStructure (shapes:array<Shape>) : BVHStructure = 
        if debugBuild then printfn "buildStructure -> started."
        if shapes.Length = 0 then failwith "buildStructure -> Unable to build BVH Tree, lists is empty."

        let boxes = convertShapesToBBoxes shapes // Get bounding boxes
        if debugBuild then printfn "buildStructure -> boxes len %i" boxes.Length

        let boxIntList = [0..boxes.Length-1]
        let rec innerNode (intIndexes:list<int>) (depthLevel:int) : BVHStructure = 
            if debugBuild then printfn "buildStructure -> innerNode"
            let boxArr = getBoxArrFromIndexes intIndexes boxes
            if debugBuild then printfn "buildStructure -> boxArr len %i" boxArr.Length
            let lowPoint, highPoint = findOuterBoundingBoxLowHighPoints boxArr
            let axisToSplit, _ = findLargestBoundingBoxSideLengths (lowPoint, highPoint)
            if debugSort then printfn "buildStructure -> axisToSplit val %i" axisToSplit
            let box = BBox (lowPoint, highPoint)
            
            let depthLevel = depthLevel + 1
            
            if debugSort then printfn "####################################"
            if debugSort then printfn "buildStructure -> boxArr %A" boxArr
            if debugSort then printfn "####################################"
            if debugSort then printfn "buildStructure -> intIndexes %A" intIndexes
            if debugSort then printfn "####################################"
            let sortedList = sortListByAxis intIndexes boxes axisToSplit // Sort list min to max
            if debugSort then printfn "####################################"
            if debugSort then printfn "buildStructure -> sortedList %A" sortedList
            if debugSort then printfn "####################################"

            if debugBuild then printfn "buildStructure -> sortedList len %i" sortedList.Length
            if debugBuild then printfn "buildStructure -> intIndexes len %i" intIndexes.Length
            match intIndexes with
            | [] -> failwith " innerNode -> Empty array"
            | b when intIndexes.Length > 1 ->
                if debugBuild then printfn "buildStructure -> Start splitting sortedList"
                let middle = sortedList.Length/2
                let leftList = sortedList.[0..middle-1]
                let rigthList = sortedList.[middle..]
                if debugBuildCounts then totalNodes <- totalNodes+1
                if debugBuild then printfn "buildStructure -> Create node"
                Node (
                            innerNode leftList depthLevel, 
                            innerNode rigthList depthLevel, 
                            box, 
                            axisToSplit)
            | c when intIndexes.Length = 1 ->
                if debugBuildCounts then totalLeafs <- totalLeafs+1
                if debugBuild then printfn "buildStructure -> Create leaf"
                Leaf (c, box)
            | [_] -> failwith "buildBVHTree -> innerNodeTree: Not caught by matching."
        
        innerNode boxIntList 0
 
    let build (shapes:array<Shape>) : BVHStructure = 
        let structure = buildStructure shapes
        if debugBuildCounts then printfn "totalNodes: %i" totalNodes 
        if debugBuildCounts then printfn "totalLeafs: %i" totalLeafs
        structure
    

 // ######################### TRAVERSAL BVH STRUCTURE #########################
    // Function swaps the order if d is not positive
    let order d (left:BVHStructure) (right:BVHStructure) = 
        match d with
        | d when d > 0 -> (left, right)
        | _            -> (right, left)

    // Function checking for leaf.
    let isLeaf = function
    | Leaf (_,_) -> true
    | _ -> false

    // Function get ray direction based on the axis.
    let getRayDirectionValue (ray:Ray) (axis:int) : int =
        match axis with
        | 0 -> (int ray.GetDirection.X)
        | 1 -> (int ray.GetDirection.Y)
        | 2 -> (int ray.GetDirection.Z)
        | _ -> invalidArg "Input out of bound" "Axis (x, y , z) paramter must 0, 1 or 2"

    // Get bounding box from structure element.
    let getBbox (structure:BVHStructure) : BBox = 
        match structure with
        | Node (_,_,bbox,_) ->  bbox
        | Leaf (_,bbox) ->      bbox

    // Functions finds closest hit of a ray in structure.
    let closestHit (structure:BVHStructure) (ray:Ray) (shapes:array<Shape>) : HitPoint option =
        match structure with
        |   Leaf (shapesRef, _) ->  let mutable closestHit = None
                                    let mutable closestDist = infinity
                                    for shapeRef in shapesRef do
                                        let hit = shapes.[shapeRef].hitFunction ray
                                        let dist = hit.Time
                                        if hit.DidHit && dist < closestDist then
                                            closestDist <- dist
                                            closestHit <- Some hit
                                    closestHit
        | _ ->  None

    // Function performs recursive searh in the structrue, with a maximum distance from the ray origin.
    let rec searchStructure (structure:BVHStructure) (ray:Ray) (shapes:array<Shape>) (tmax:float) =
        let nodeBBox = getBbox structure
        match nodeBBox.intersect ray with  
        | Some (t, t')  ->  
                        if t<tmax then 
                            if isLeaf structure then
                                let checkForHit = (closestHit structure ray shapes)
                                match checkForHit with
                                | Some hitPoint when hitPoint.Time<tmax -> Some hitPoint
                                | _ -> None
                            else 
                                match structure with
                                | Node (left, right, _, axis) ->
                                    let dir = getRayDirectionValue ray axis
                                    let fst, snd = order dir left right
                                    match searchStructure fst ray shapes tmax with
                                    | Some hitPointfst ->
                                            let someResult = searchStructure structure ray shapes hitPointfst.Time
                                            match someResult with
                                            | Some hitPointSec -> Some hitPointSec
                                            | _ -> Some hitPointfst            
                                    | _ -> searchStructure snd ray shapes tmax
                                | _ -> None
                        else None
        | None -> None
    
    // Function for traversal of the structure.
    let traverse (structure:BVHStructure) (ray:Ray) (shapes:array<Shape>) : HitPoint = 
        match searchStructure structure ray shapes infinity with
        | Some r -> r
        | None -> HitPoint ray