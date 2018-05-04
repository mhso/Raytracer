namespace Tracer

module BVH = 
    open Tracer.Basics
    
    let debug = true

    (* BVH TREE *)
    type BVHtree = 
        | Leaf of List<int> * BBox
        | Node of BVHtree * BVHtree * BBox * int

    let rec sortListByAxis (indexList:list<int>) (boxes:array<BBox>) (axis:int) =
      match indexList with
      | [] -> []
      | x :: xs ->
          let small, large = 
              match axis with
              | 0 ->
                    let filterSmall = fun e -> boxes.[e].lowPoint.X <= boxes.[x].lowPoint.X
                    let filterLarger = fun e -> boxes.[e].lowPoint.X >  boxes.[x].lowPoint.X
                    filterSmall, filterLarger
              | 1 -> 
                    let filterSmall = fun e -> boxes.[e].lowPoint.Y <= boxes.[x].lowPoint.Y
                    let filterLarger = fun e -> boxes.[e].lowPoint.Y >  boxes.[x].lowPoint.Y
                    filterSmall, filterLarger
              | _ ->
                    let filterSmall = fun e -> boxes.[e].lowPoint.Z <= boxes.[x].lowPoint.Z
                    let filterLarger = fun e -> boxes.[e].lowPoint.Z >  boxes.[x].lowPoint.Z
                    filterSmall, filterLarger

          let smaller = sortListByAxis (xs |> List.filter(small)) (boxes) axis
          let larger  = sortListByAxis (xs |> List.filter(large)) (boxes) axis
          smaller @ [x] @ larger
    
    let findOuterBoundingBoxLowHighPoints (boxes:array<BBox>) = 
        let lowX = Array.fold (fun acc (box:BBox) -> if box.lowPoint.X < acc then box.lowPoint.X else acc) infinity boxes
        let lowY = Array.fold (fun acc (box:BBox) -> if box.lowPoint.Y < acc then box.lowPoint.Y else acc) infinity boxes
        let lowZ = Array.fold (fun acc (box:BBox) -> if box.lowPoint.Z > acc then box.lowPoint.Z else acc) -infinity boxes
        let highX = Array.fold (fun acc (box:BBox) -> if box.highPoint.X > acc then box.highPoint.X else acc) -infinity boxes
        let highY = Array.fold (fun acc (box:BBox) -> if box.highPoint.Y > acc then box.highPoint.Y else acc) -infinity boxes
        let highZ = Array.fold (fun acc (box:BBox) -> if box.highPoint.Z < acc then box.highPoint.Z else acc) infinity boxes
        
        Point(lowX, lowY, lowZ), Point(highX, highY, highZ)

    let findLargestBoundingBoxSideLengths (box:(Point*Point)) =
        let lowPoint, highPoint = box
        
        let x = highPoint.X - lowPoint.X
        let y = highPoint.Y - lowPoint.Y
        let z = highPoint.Z - lowPoint.Z
        let mutable t = 0.
        let mutable value = (0, 0.)

        if x > t then
            value <- (0, x)
        if y > t then
            value <- (1, y)
        if y < z then
            value <- (2, z)
        value

    let findAxisMinMaxValues (bBox:BBox) axis =
        let (lowPoint, highXYZ) = (bBox.lowPoint, bBox.highPoint);
        match axis with
        | 0 -> (lowPoint.X, highXYZ.X)
        | 1 -> (lowPoint.Y, highXYZ.Y)
        | 2 -> (lowPoint.Z, highXYZ.Z)
        | _ -> invalidArg "findAxisMinMaxValues invalid axis value" "Axis value needs to be between 0-2."
    
    let rec getBoxArrFromIndexes (indexes:list<int>) (boxes:array<BBox>) : (array<BBox>) =
        [|for i in 0..(indexes.Length-1) -> boxes.[i]|]
        
 
 // ######################### BUILD BVH TREE #########################
    let buildBVHTree (boxes:array<BBox>) : BVHtree = 
        if boxes.Length = 0 then failwith "Unable to build BVH Tree, lists is empty."
        let boxIntList = [0..boxes.Length-1]
        let rec innerNodeTree (intIndexes:list<int>) (treeLevel:int) : BVHtree = 
            let boxArr = getBoxArrFromIndexes intIndexes boxes
            let lowPoint, highPoint = findOuterBoundingBoxLowHighPoints boxArr
            let axisToSplit, _ = findLargestBoundingBoxSideLengths (lowPoint, highPoint)
            let box = BBox (lowPoint, highPoint)
            let treeLevel = treeLevel + 1
            //printfn "innerNodeTree rec run... axisToSplit: %i, countRuns: %i" axisToSplit (treeLevel)
            let sortedList = sortListByAxis intIndexes boxes axisToSplit
            match intIndexes with
            | [] -> failwith " innerNodeTree -> Empty array"
            | b when intIndexes.Length > 1 ->
                let middle = sortedList.Length/2
                let leftList = sortedList.[0..middle-1]
                let rigthList = sortedList.[middle..]
                
                //printfn "innerNodeTree rec run... axisToSplit: %i, countRuns: %i" axisToSplit (treeLevel)
                //printfn "Add new inner Nodes... Lists lenght: "
                //printfn "intIndexes.Length: %i " intIndexes.Length
                //printfn "boxArr.Length: %i " boxArr.Length
                //printfn "leftList.Length: %i " leftList.Length
                //printfn "rigthList.Length: %i " rigthList.Length

                Node (
                            innerNodeTree leftList treeLevel, 
                            innerNodeTree rigthList treeLevel, 
                            box, 
                            axisToSplit)
            | c when intIndexes.Length = 1 ->
                //printfn "Add new inner Leaf... Value: %O" c
                Leaf (c, box)
            | [_] -> failwith "buildBVHTree -> innerNodeTree: Not caught by matching."
        innerNodeTree boxIntList 0
 
 
  // ######################### TRAVERSAL BVH TREE #########################

    // swaps the order if d is not positive
    let order d (left:BVHtree) (right:BVHtree) = 
        if debug then printfn "Call to order..."
        match d with
        | d when d > 0 -> (left, right)
        | _            -> (right, left)

    // Checking for tree leaf.
    let isLeaf = function
    | Leaf (_,_) -> true
    | _ -> false

    let getRayDirectionValue (ray:Ray) (axis:int) =
        if debug then printfn "Call to getRayDirectionValue..."
        match axis with
        | 0 -> (int ray.GetDirection.X)
        | 1 -> (int ray.GetDirection.Y)
        | 2 -> (int ray.GetDirection.Z)
        | _ -> invalidArg "Input out of bound" "Axis (x, y , z) paramter must 0, 1 or 2"

    // Get bounding box from tree element.
    let getBbox (tree:BVHtree) : BBox = 
        match tree with
        | Node (_,_,bbox,_) ->  if debug then printfn "getBox -> Node box \n %A" bbox
                                bbox
        | Leaf (_,bbox) ->      if debug then printfn "getBox -> Leaf box \n %A" bbox
                                bbox

    let closestHit (treeNode:BVHtree) (ray:Ray) (shapes:array<Shape>)  =
        if debug then printfn "Call to closestHit..."
        match treeNode with
        | Leaf (shapesRef, _) -> 
                            let mutable closestHit = None
                            let mutable closestDist = infinity
                            for shapeRef in shapesRef do
                                let hit = shapes.[shapeRef].hitFunction ray
                                let dist = hit.Time
                                if dist < closestDist then
                                    closestDist <- dist
                                    closestHit <- Some hit
                            if debug then printfn "closestHit -> Leaf found return hit at dist %f" closestDist
                            closestHit
        | _ -> 
                if debug then printfn "closestHit -> None..."
                None

    let rec search (treeNode:BVHtree) (ray:Ray) (shapes:array<Shape>) (tmax:float) =
        if debug then printfn "Call to search with tmax: %f, lenght of array %i" tmax shapes.Length 
        if debug then printfn "Value of ray: %A" ray
        if debug then printfn "Value of treeNode: %A" treeNode
        let treeNodeBBox = getBbox treeNode
        if debug then printfn "Value of treeNodeBBox: \n %A" treeNodeBBox
        let value = treeNodeBBox.intersect ray
        if value.IsSome then printfn "search -> Intersect is Some..."
        if value.IsNone then printfn "search -> Intersect is None..."
        if debug then printfn "Value of intersect: \n %A" value
        match value with  
        | Some (t, t')  -> 
                            printfn "match value -> Some (t, t') : t = %f  t' = %f" t t'
                            
                            if (t<tmax) then 
                                if isLeaf treeNode then
                                    if debug then printfn "(t<tmax) and isLeaf..."
                                    let checkForHit = (closestHit treeNode ray shapes)
                                    match checkForHit with
                                    | Some hitFound -> 
                                        if hitFound.Time < tmax then Some hitFound
                                        else None
                                    | None -> None
                                else 
                                    match treeNode with
                                    | Node (left, right, _, _) ->
                                        let first = search left ray shapes tmax
                                        let second = search right ray shapes tmax
                                        match first with
                                        | Some hitFound1 ->
                                                let result2 = search treeNode ray shapes hitFound1.Time
                                                match result2 with
                                                | Some hitFound2 -> Some hitFound2
                                                | _ -> Some hitFound1
            
                                        | None -> second
                                    | _ -> None
                            else None
        | None -> if debug then printfn "search -> match value -> None"
                  None

    let traverse (treeNode:BVHtree) (ray:Ray) (shapes:array<Shape>) (tmax:float) = 
        if debug then printfn "Call to traverse..."
        search treeNode ray shapes infinity
        
        