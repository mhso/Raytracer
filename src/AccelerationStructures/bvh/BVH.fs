namespace Tracer
open Tracer.Basics

module BVH = 
    open System
    open Tracer

    (* BVH TREE *)
    type BBox = { lowXYZ:Point; 
                  highXYZ:Point;
                }

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
                    let filterSmall = fun e -> boxes.[e].lowXYZ.X <= boxes.[x].lowXYZ.X
                    let filterLarger = fun e -> boxes.[e].lowXYZ.X >  boxes.[x].lowXYZ.X
                    filterSmall, filterLarger
              | 1 -> 
                    let filterSmall = fun e -> boxes.[e].lowXYZ.Y <= boxes.[x].lowXYZ.Y
                    let filterLarger = fun e -> boxes.[e].lowXYZ.Y >  boxes.[x].lowXYZ.Y
                    filterSmall, filterLarger
              | _ ->
                    let filterSmall = fun e -> boxes.[e].lowXYZ.Z <= boxes.[x].lowXYZ.Z
                    let filterLarger = fun e -> boxes.[e].lowXYZ.Z >  boxes.[x].lowXYZ.Z
                    filterSmall, filterLarger

          let smaller = sortListByAxis (xs |> List.filter(small)) (boxes) axis
          let larger  = sortListByAxis (xs |> List.filter(large)) (boxes) axis
          smaller @ [x] @ larger
    
    let findOuterBoundingBoxLowHighPoints (boxes:array<BBox>) = 
        let lowX = Array.fold (fun acc box -> if box.lowXYZ.X < acc then box.lowXYZ.X else acc) infinity boxes
        let lowY = Array.fold (fun acc box -> if box.lowXYZ.Y < acc then box.lowXYZ.Y else acc) infinity boxes
        let lowZ = Array.fold (fun acc box -> if box.lowXYZ.Z > acc then box.lowXYZ.Z else acc) -infinity boxes
        let highX = Array.fold (fun acc box -> if box.highXYZ.X > acc then box.highXYZ.X else acc) -infinity boxes
        let highY = Array.fold (fun acc box -> if box.highXYZ.Y > acc then box.highXYZ.Y else acc) -infinity boxes
        let highZ = Array.fold (fun acc box -> if box.highXYZ.Z < acc then box.highXYZ.Z else acc) infinity boxes
        
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
        let (lowXYZ, highXYZ) = (bBox.lowXYZ, bBox.highXYZ);
        match axis with
        | 0 -> (lowXYZ.X, highXYZ.X)
        | 1 -> (lowXYZ.Y, highXYZ.Y)
        | 2 -> (lowXYZ.Z, highXYZ.Z)
        | _ -> invalidArg "findAxisMinMaxValues invalid axis value" "Axis value needs to be between 0-2."
    
    let rec getBoxArrFromIndexes (indexes:list<int>) (boxes:array<BBox>) : (array<BBox>) =
        [|for i in 0..(indexes.Length-1) -> boxes.[i]|]
        
 
    //Temporary Intersect-function. Use Alexanders when available.
    let intersect (box:BBox)(r:Ray) = 
        let tx = if r.GetDirection.X >= 0.0 then (box.lowXYZ.X - r.GetOrigin.X)/r.GetDirection.X else (box.highXYZ.X - r.GetOrigin.X)/r.GetDirection.X
        let tx' = if r.GetDirection.X >= 0.0 then (box.highXYZ.X - r.GetOrigin.X)/r.GetDirection.X else (box.lowXYZ.X - r.GetOrigin.X)/r.GetDirection.X
        let ty = if r.GetDirection.Y >= 0.0 then (box.lowXYZ.Y - r.GetOrigin.Y)/r.GetDirection.Y else (box.highXYZ.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let ty' = if r.GetDirection.Y >= 0.0 then (box.highXYZ.Y - r.GetOrigin.Y)/r.GetDirection.Y else (box.lowXYZ.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let tz = if r.GetDirection.Z >= 0.0 then (box.lowXYZ.Z - r.GetOrigin.Z)/r.GetDirection.Z else (box.highXYZ.Z - r.GetOrigin.Z)/r.GetDirection.Z
        let tz' = if r.GetDirection.Z >= 0.0 then (box.highXYZ.Z - r.GetOrigin.Z)/r.GetDirection.Z else (box.lowXYZ.Z - r.GetOrigin.Z)/r.GetDirection.Z

        let t = max tx (max ty tz)
        let t' = min tx' (min ty' tz')
        if t < t' && t' > 0.0 then Some(t, t')
        else None

    let buildBVHTree (boxes:array<BBox>) : BVHtree = 
        if boxes.Length = 0 then failwith "Unable to build BVH Tree, lists is empty."
        let boxIntList = [0..boxes.Length-1]
        let rec innerNodeTree (intIndexes:list<int>) (treeLevel:int) : BVHtree = 
            let boxArr = getBoxArrFromIndexes intIndexes boxes
            let lowPoint, highPoint = findOuterBoundingBoxLowHighPoints boxArr
            let axisToSplit, _ = findLargestBoundingBoxSideLengths (lowPoint, highPoint)
            let box = {  lowXYZ = lowPoint;
                             highXYZ = highPoint;
                    } 
            let treeLevel = treeLevel + 1
            //printfn "innerNodeTree rec run... axisToSplit: %i, countRuns: %i" axisToSplit (treeLevel)
            let sortedList = sortListByAxis intIndexes boxes axisToSplit
            match intIndexes with
            | [] -> failwith " innerNodeTree -> Empty array"
            | b when intIndexes.Length > 1 ->
                let middle = sortedList.Length/2
                let leftList = sortedList.[0..middle-1]
                let rigthList = sortedList.[middle..]
                
                printfn "innerNodeTree rec run... axisToSplit: %i, countRuns: %i" axisToSplit (treeLevel)
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
                            
    // swaps the order if d is not positive
    //type Order () = int -> Node -> Node
    let order d (left:BVHtree) (right:BVHtree) = 
        match d with
        | d when d > 0 -> (left, right)
        | _            -> (right, left)

    let getRayDirectionValue (ray:Ray) (axis:int) =
        match axis with
        | 0 -> (int ray.GetDirection.X)
        | 1 -> (int ray.GetDirection.Y)
        | 2 -> (int ray.GetDirection.Z)
        | _ -> invalidArg "Input out of bound" "Axis (x, y , z) paramter must 0, 1 or 2"

    let isLeaf = function
    | Leaf (_,_) -> true
    | _ -> false

    let getBbox (tree:BVHtree) : BBox = 
        match tree with
        | Node (_,_,bbox,_) -> bbox
        | Leaf (_, bbox) -> bbox

    let closetHit (treeNode:BVHtree) (ray:Ray) (shapes:array<Shape>) =
        match treeNode with
        | Leaf (shapesRef, _) -> 
                            let mutable closestShape = None
                            let mutable closestDist = infinity
                            for shapeRef in shapesRef do
                                let hit = shapes[shapeRef].hitFunction ray
                                let dist = hit.time
                                if dist < closestDist then
                                    closestDist <- dist
                                    closestShape <- 
        | _ -> None

    let rec search (treeNode:BVHtree) (ray:Ray) (tmax:float) : 'T option =     
        let value = intersect (getBbox treeNode) ray
        match value with  
        | Some (t, t')  -> if (t<tmax) then 
                                if (isLeaf treeNode) == true then
                                    if closestHit(treeNode, ray) = Some hit ^ hit.distance < tmax then
                                        Some hit
                                //else 
                                //    searchNode (treeNode ray tmax)
        | None -> None
    //and searchNode (tree:BVHtree) (ray:Ray) tmax : 'T option =
    //    match tree with
    //    | Node (nleft, nright, bbox, naxis) ->
    //        let fst, snd = order (ray.GetDirection nleft nright)
    //        if search(fst, ray, tmax) = Some hit then
    //            if search(fst, ray, hit.distance) = Some hit' then
    //                Some hit'
    //            else
    //                Some HitPoint
    //        else
    //                search (snd ray tmax)
    //    | _ -> None

    let traverse(bvh, ray) = 
        search bvh ray infinity
        
        