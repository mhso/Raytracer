namespace Tracer
open Tracer.Basics

module BVH = 

    //#load Vector.fs
    //#load Point.fs

    (* TEMP SHAPE *)
    type Shape = S of float

    (* BVH TREE *)
    type BBox = { lowXYZ:Point; 
                  highXYZ:Point;
                }

    (* BVH TREE *)
    type BVHtree = 
        | Leaf of List<int>  
        | Node of option<BVHtree> * option<BVHtree> * BBox * int

    let rec sortListByAxis (xs:list<BBox>) (axis:int) =
      match xs with
      | [] -> []
      | x :: xs ->
          let small, large = 
              match axis with
              | 0 ->
                    let filterSmall = fun e -> e.lowXYZ.X <= x.lowXYZ.X
                    let filterLarger = fun e -> e.lowXYZ.X >  x.lowXYZ.X
                    filterSmall, filterLarger
              | 1 -> 
                    let filterSmall = fun e -> e.lowXYZ.Y <= x.lowXYZ.Y
                    let filterLarger = fun e -> e.lowXYZ.Y >  x.lowXYZ.Y
                    filterSmall, filterLarger
              | _ ->
                    let filterSmall = fun e -> e.lowXYZ.Z <= x.lowXYZ.Z
                    let filterLarger = fun e -> e.lowXYZ.Z >  x.lowXYZ.Z
                    filterSmall, filterLarger

          let smaller = sortListByAxis (xs |> List.filter(small)) axis
          let larger  = sortListByAxis (xs |> List.filter(large)) axis
          smaller @ [x] @ larger
    
    let findOuterBoundingBoxLowHighPoints (xs:list<BBox>) = 
        let lowX = List.fold (fun acc box -> if box.lowXYZ.X < acc then box.lowXYZ.X else acc) infinity xs
        let lowY = List.fold (fun acc box -> if box.lowXYZ.Y < acc then box.lowXYZ.Y else acc) infinity xs
        let lowZ = List.fold (fun acc box -> if box.lowXYZ.Z > acc then box.lowXYZ.Z else acc) -infinity xs
        let highX = List.fold (fun acc box -> if box.highXYZ.X > acc then box.highXYZ.X else acc) -infinity xs
        let highY = List.fold (fun acc box -> if box.highXYZ.Y > acc then box.highXYZ.Y else acc) -infinity xs
        let highZ = List.fold (fun acc box -> if box.highXYZ.Z < acc then box.highXYZ.Z else acc) infinity xs
        
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

    let findAxisMinMaxValues (bBox : BBox) axis =
        let (lowXYZ, highXYZ) = (bBox.lowXYZ, bBox.highXYZ);
        match axis with
        | 0 -> (lowXYZ.X, highXYZ.X)
        | 1 -> (lowXYZ.Y, highXYZ.Y)
        | 2 -> (lowXYZ.Z, highXYZ.Z)
        | _ -> invalidArg "findAxisMinMaxValues invalid axis value" "Axis value needs to be between 0-2."
        

    let buildBVHTree (xs:list<BBox>) : option<BVHtree> = 
        if xs.Length = 0 then failwith "Unable to build BVH Tree, lists is empty."

        let rec innerBVHTree (xs:list<BBox>) (lastAxis:int) : option<BVHtree> = 
            let lowPoint, highPoint = findOuterBoundingBoxLowHighPoints xs
            let axisToSplit, _ = findLargestBoundingBoxSideLengths (lowPoint, highPoint)
            let sortedList = sortListByAxis xs axisToSplit
            printfn "List: %A" xs
            printfn  "LastAxis: %i" lastAxis
            
            match xs with
            | [] ->  None
            | x when xs.Length > 1 ->
                let middle = sortedList.Length/2
                let leftList = sortedList.[0..middle]
                let rigthList = sortedList.[middle+1..]

                printfn "middle: %i" middle
                printfn "leftList: %A" leftList
                printfn "rigthList: %A" rigthList
                let box = { lowXYZ=lowPoint; highXYZ=highPoint }
                let axisMinMaxBounding = findAxisMinMaxValues (box) axisToSplit

                Some (Node (
                            innerBVHTree leftList (axisToSplit), 
                            innerBVHTree rigthList (axisToSplit), 
                            box, 
                            axisToSplit))
            | _ -> Some (Leaf [xs.[0]])

        innerBVHTree xs 0
            


        

    // swaps the order if d is not positive
    // type Order () = int -> Node -> Node
    //let order d left right = 
    //    match d with
    //    | d when d > 0 -> (left, right)
    //    | _            -> (right, left)

    // let search node ray tmax = Some hit

    //let getRayDirectionValue (ray:Ray) (axis:int) =
    //    match axis with
    //    | 0 -> (int ray.GetDirection.X)
    //    | 1 -> (int ray.GetDirection.Y)
    //    | 2 -> (int ray.GetDirection.Z)
    //    | _ -> invalidArg "Input out of bound" "Axis (x, y , z) paramter must 0, 1 or 2"

    //let searchNode node ray tmax = 
    //    let (fst, snd) = order (getRayDirectionValue ray getAxis node) node.left node.right
    //    (fst, snd)
    // if search(fst, ray, tmax) = Some hit then
    //    if search(fst, ray, hit.distance) = Some hit2 then
    //     Some hit2
    //    else
    //     Some hit
    // else
    //     search(snd, ray tmax)


    //let isLeaf input =
    //    match input with
    //    | input when (input :? Leaf) -> true
    //    | _ -> false

    //let isClosetHit [] ray = failWith "Not implemented"

    //let intersect bbox ray = failWith "Not implemented"

    //let traverse(bvh, ray) = failWith "Not implemented" //search(bvh.root, ray, 1)

    //let search node ray tmax = failWith "Not implemented"
        