namespace Tracer.Basics

module BVH = 
    
    // Used for debug, will print to console etc. 
    let debug = false

    // Type of the BVHTree, with Nodes and Leafs.
    type BVHStructure = | Leaf of List<int>*BBox
                        | Node of BVHStructure*BVHStructure*BBox*int
    
    // Function for sorting a int list, based on array of bounding boxes and axis value.
    let rec sortListByAxis (indexList:list<int>) (boxes:array<BBox>) (axis:int) =
      match indexList with
      | [] -> []
      | x :: xs ->
          let less, great = 
              match axis with
              // Sort by x axis
              | 0 -> let filterLess = fun e -> boxes.[e].lowPoint.X <= boxes.[x].lowPoint.X
                     let filterGreat = fun e -> boxes.[e].lowPoint.X >  boxes.[x].lowPoint.X
                     filterLess, filterGreat
              // Sort by y axis
              | 1 -> let filterLess = fun e -> boxes.[e].lowPoint.Y <= boxes.[x].lowPoint.Y
                     let filterGreat = fun e -> boxes.[e].lowPoint.Y >  boxes.[x].lowPoint.Y
                     filterLess, filterGreat
              // Sort by z axis
              | _ -> let filterLess = fun e -> boxes.[e].lowPoint.Z <= boxes.[x].lowPoint.Z
                     let filterGreat = fun e -> boxes.[e].lowPoint.Z >  boxes.[x].lowPoint.Z
                     filterLess, filterGreat

          let lesser    = sortListByAxis (xs |> List.filter(less)) (boxes) axis
          let greater   = sortListByAxis (xs |> List.filter(great)) (boxes) axis
          lesser @ [x] @ greater
    
    let rec quickselect (k:int) (indexList:list<int>) (boxes:list<BBox>) (axis:int) = 
        match boxes with
        | [] -> failwith "Cannot take largest element of empty list."
        | [a] -> a
        | x::xs ->
            match axis with
            | 0 ->  let (ys, zs) = List.partition (fun (arg:BBox) -> arg.highPoint.X < x.highPoint.X) xs
                    let l = List.length ys
                    if k < l then quickselect k indexList ys axis
                    elif k > l then quickselect (k-l-1) indexList zs axis
                    else x
            | 1 ->  let (ys, zs) = List.partition (fun (arg:BBox) -> arg.highPoint.Y < x.highPoint.Y) xs
                    let l = List.length ys
                    if k < l then quickselect k indexList ys axis
                    elif k > l then quickselect (k-l-1) indexList zs axis
                    else x
            | 2 ->  let (ys, zs) = List.partition (fun (arg:BBox) -> arg.highPoint.Z < x.highPoint.Z) xs
                    let l = List.length ys
                    if k < l then quickselect k indexList ys axis
                    elif k > l then quickselect (k-l-1) indexList zs axis
                    else x

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
    let findLargestBoundingBoxSideLengths (box:(Point*Point)) =
        let lowPoint, highPoint = box
        let x = highPoint.X - lowPoint.X
        let y = highPoint.Y - lowPoint.Y
        let z = highPoint.Z - lowPoint.Z
        let mutable t = 0.
        let mutable value = (0, 0.)

        if x > t then value <- (0, x)
        if y > t then value <- (1, y)
        if y < z then value <- (2, z)
        value
    
    // Function to find the min-values from a bounding box based on the axis value.
    let findAxisMinMaxValues (bBox:BBox) axis =
        let lowPoint, highPoint = bBox.lowPoint, bBox.highPoint;
        match axis with
        | 0 -> (lowPoint.X, highPoint.X)
        | 1 -> (lowPoint.Y, highPoint.Y)
        | 2 -> (lowPoint.Z, highPoint.Z)
        | _ -> invalidArg "findAxisMinMaxValues invalid axis value" "Axis value needs to be between 0-2."
    
    let rec getBoxArrFromIndexes (indexes:list<int>) (boxes:array<BBox>) : (array<BBox>) =
        [|for i in 0..(indexes.Length-1) -> boxes.[indexes.[i]]|]
    
    // Function for converting a list of shapes to an array of their bounding boxes.
    let convertShapesToBBoxes (shapes:array<Shape>) : array<BBox> =
        let bboxArr : BBox[] = Array.zeroCreate (shapes.Length)
        for i in 0..bboxArr.Length-1 do
            bboxArr.[i] <- shapes.[i].getBoundingBox()
        bboxArr

 // ######################### BUILD BVH TREE #########################

    // Build BVH structure from a list of shapes.
    let build (shapes:array<Shape>) : BVHStructure = 
        if shapes.Length = 0 then failwith "Unable to build BVH Tree, lists is empty."

        let boxes = convertShapesToBBoxes shapes

        let boxIntList = [0..boxes.Length-1]
        let rec innerNode (intIndexes:list<int>) (depthLevel:int) : BVHStructure = 
            let boxArr = getBoxArrFromIndexes intIndexes boxes
            let lowPoint, highPoint = findOuterBoundingBoxLowHighPoints boxArr
            let axisToSplit, _ = findLargestBoundingBoxSideLengths (lowPoint, highPoint)
            let box = BBox (lowPoint, highPoint)
            let depthLevel = depthLevel + 1
            if debug then printfn "innerNodeTree rec run... axisToSplit: %i, countRuns: %i" axisToSplit (depthLevel)
            let sortedList = sortListByAxis intIndexes boxes axisToSplit

            //let quickSorted = quickselect 0 intIndexes (Array.toList boxes) axisToSplit
            //printfn "BVH quickSorted: %A" quickSorted

            match intIndexes with
            | [] -> failwith " innerNode -> Empty array"
            | b when intIndexes.Length > 1 ->
                let middle = sortedList.Length/2
                let leftList = sortedList.[0..middle-1]
                let rigthList = sortedList.[middle..]
                Node (
                            innerNode leftList depthLevel, 
                            innerNode rigthList depthLevel, 
                            box, 
                            axisToSplit)
            | c when intIndexes.Length = 1 ->
                if debug then printfn "Add new inner Leaf... Value: %O" c
                Leaf (c, box)
            | [_] -> failwith "buildBVHTree -> innerNodeTree: Not caught by matching."
        innerNode boxIntList 0
 
  // ######################### TRAVERSAL BVH STRUCTURE #########################

    // Function swaps the order if d is not positive
    let order d (left:BVHStructure) (right:BVHStructure) = 
        if debug then printfn "Call to order..."
        match d with
        | d when d > 0 -> (left, right)
        | _            -> (right, left)

    // Function checking for leaf.
    let isLeaf = function
    | Leaf (_,_) -> true
    | _ -> false

    // Function get ray direction based on the axis.
    let getRayDirectionValue (ray:Ray) (axis:int) =
        if debug then printfn "Call to getRayDirectionValue..."
        match axis with
        | 0 -> (int ray.GetDirection.X)
        | 1 -> (int ray.GetDirection.Y)
        | 2 -> (int ray.GetDirection.Z)
        | _ -> invalidArg "Input out of bound" "Axis (x, y , z) paramter must 0, 1 or 2"

    // Get bounding box from structure element.
    let getBbox (structure:BVHStructure) : BBox = 
        match structure with
        | Node (_,_,bbox,_) ->  if debug then printfn "getBox -> Node box \n %A" bbox
                                bbox
        | Leaf (_,bbox) ->      if debug then printfn "getBox -> Leaf box \n %A" bbox
                                bbox

    // Functions finds closest hit of a ray in structure.
    let closestHit (structure:BVHStructure) (ray:Ray) (shapes:array<Shape>)  =
        if debug then printfn "Call to closestHit..."
        match structure with
        |   Leaf (shapesRef, _) ->  let mutable closestHit = None
                                    let mutable closestDist = infinity
                                    for shapeRef in shapesRef do
                                        let hit = shapes.[shapeRef].hitFunction ray
                                        let dist = hit.Time
                                        if hit.DidHit && dist < closestDist then
                                            closestDist <- dist
                                            closestHit <- Some hit
                                    if debug then printfn "closestHit -> Leaf found return hit at dist %f" closestDist
                                    closestHit
        | _ ->  if debug then printfn "closestHit -> None..."
                None

    // Function performs recursive searh in the structrue, with a maximum distance from the ray origin.
    let rec search (structure:BVHStructure) (ray:Ray) (shapes:array<Shape>) (tmax:float) =
        if debug then printfn "Call to search with tmax: %f, lenght of array %i" tmax shapes.Length 
        if debug then printfn "Value of ray GetOrigin: %A GetDirection: %A" ray.GetOrigin ray.GetDirection
        //if debug then printfn "Value of structure: %A" structure
        let nodeBBox = getBbox structure
        if debug then printfn "Value of nodeBBox: \n %A" nodeBBox
        let value = nodeBBox.intersect ray
        if debug then printfn "Value of intersect: \n %A" value.IsSome
        match value with  
        | Some (t, t')  ->  if debug then printfn "match value -> Some (t, t') : t = %f  t' = %f" t t'       
                            if (t<tmax) then 
                                if debug then printfn "Value -> Some -> (t<tmax)... Structure: %A" structure 
                                if isLeaf structure then
                                    if debug then printfn "(t<tmax) and isLeaf..."
                                    let checkForHit = (closestHit structure ray shapes)
                                    match checkForHit with
                                    | Some hitFound -> 
                                        if hitFound.Time < tmax then Some hitFound
                                        else None
                                    | _ -> None
                                else 
                                    if debug then printfn "Value -> Some -> and NOT isLeaf..."
                                    match structure with
                                    | Node (left, right, _, _) ->
                                        if debug then printfn "Value -> Some -> Left structure: %A" left
                                        if debug then printfn "Value -> Some -> Right structure: %A" right 
                                        let first = search left ray shapes tmax
                                        let second = search right ray shapes tmax
                                        match first with
                                        | Some hitFound1 ->
                                                let result2 = search structure ray shapes hitFound1.Time
                                                match result2 with
                                                | Some hitFound2 -> Some hitFound2
                                                | _ -> Some hitFound1            
                                        | _ -> second
                                    | _ -> None
                            else None
        | None -> if debug then printfn "search -> match value -> None"
                  None
    
    // Function for traversal of the structure.
    let traverse (structure:BVHStructure) (ray:Ray) (shapes:array<Shape>) = 
        //printfn "traverse structure: %A" structure
        let result = search structure ray shapes infinity
        match result with
        | Some r -> r
        | None -> HitPoint ray