namespace Tracer
open Tracer.Basics

module BVH = 

    //#load Vector.fs
    //#load Point.fs

    //(* Common types *)
    //type Axis = A of int

    (* SHAPE *)
    type Shape = S of float

    type Coordinate = { x:float; y:float; z:float }

    type BBox = { lowXYZ:Coordinate; 
                  highXYZ:Coordinate;
                  shape:Shape }

    let rec sortListByAxis (xs:list<BBox>) (axis:int) =
      match xs with
      | [] -> []
      | x :: xs ->
          let small, large = 
              match axis with
              | 0 ->
                    let filterSmall = fun e -> e.lowXYZ.x <= x.lowXYZ.x
                    let filterLarger = fun e -> e.lowXYZ.x >  x.lowXYZ.x
                    filterSmall, filterLarger
              | 1 -> 
                    let filterSmall = fun e -> e.lowXYZ.y <= x.lowXYZ.y
                    let filterLarger = fun e -> e.lowXYZ.y >  x.lowXYZ.y
                    filterSmall, filterLarger
              | _ ->
                    let filterSmall = fun e -> e.lowXYZ.z <= x.lowXYZ.z
                    let filterLarger = fun e -> e.lowXYZ.z >  x.lowXYZ.z
                    filterSmall, filterLarger

          let smaller = sortListByAxis (xs |> List.filter(small)) axis
          let larger  = sortListByAxis (xs |> List.filter(large)) axis
          smaller @ [x] @ larger
    
    (* Boundingbox Coords *)
    type BoundingboxCoords = Point * Point
    type AxisMinMaxBounding = float * float 

    (* BVH TREE *)
    type BVHtree = 
        | Leaf of BBox
        | Node of option<BVHtree> * option<BVHtree> * BoundingboxCoords * int
    
    let findOuterBoundingBoxLowHighPoints (xs:list<BBox>) = 
        let lowX = List.fold (fun acc box -> if box.lowXYZ.x < acc then box.lowXYZ.x else acc) infinity xs
        let lowY = List.fold (fun acc box -> if box.lowXYZ.y < acc then box.lowXYZ.y else acc) infinity xs
        let lowZ = List.fold (fun acc box -> if box.lowXYZ.z > acc then box.lowXYZ.z else acc) -infinity xs
        let highX = List.fold (fun acc box -> if box.highXYZ.x > acc then box.highXYZ.x else acc) -infinity xs
        let highY = List.fold (fun acc box -> if box.highXYZ.y > acc then box.highXYZ.y else acc) -infinity xs
        let highZ = List.fold (fun acc box -> if box.highXYZ.z < acc then box.highXYZ.z else acc) infinity xs
        
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

    let findAxisMinMaxValues (boundingboxCoords : BoundingboxCoords) axis =
        let pMin, pMax = boundingboxCoords;
        match axis with
        | 0 -> (pMin.X, pMax.X)
        | 1 -> (pMin.Y, pMax.Y)
        | 2 -> (pMin.Z, pMax.Z)
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
                    
                let axisMinMaxBounding = findAxisMinMaxValues (lowPoint, highPoint) axisToSplit

                Some (Node (
                            innerBVHTree leftList (axisToSplit), 
                            innerBVHTree rigthList (axisToSplit), 
                            axisMinMaxBounding, 
                            axisToSplit))
            | _ -> Some (Leaf xs.[0])

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
        