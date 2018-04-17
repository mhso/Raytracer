﻿namespace Tracer
open Tracer.Basics

module BVH = 

    //#load Vector.fs
    //#load Point.fs

    //(* Common types *)
    type Axis = A of int

    (* SHAPE *)
    type Shape = S of float

    type coordinate = { x:float; y:float; z:float }

    type BBox = { lowXYZ:coordinate; 
                  highXYZ:coordinate;
                  shape:Shape }

    let rec sortListByAxis (xs:list<BBox>) (axis:int) =
      match xs with
      | [] -> []
      | x :: xs ->
          let small, large = match axis with
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

    (* BVH TREE *)
    type 'shape BVHtree = 
        | Leaf of 'shape * BBox * Axis
        | Node of 'shape BVHtree * 'shape BVHtree * BBox * Axis

    //(* LEAF *)
    //let getShapes (Leaf(shapes, _, _))    = shapes
    //let getBbox (Leaf(_, bbox, _))        = bbox
    //let getAxis (Leaf(_, _, axis))        = axis

    // let testBVHTree = Node(Leaf "left", Leaf "right")
    type  BoundingboxCoords = Point * Point

    let getOuterBoundinBox (xs:list<BBox>) = 
        let sortX  = sortListByAxis xs 0
        let sortY  = sortListByAxis xs 1
        let sortZ  = sortListByAxis xs 2
        let notZero = 0.000000

        let lowPoint = Point(
                                sortX.Head.lowXYZ.x - notZero,
                                sortY.Head.lowXYZ.y - notZero,
                                sortZ.Head.lowXYZ.z - notZero )
        let highPoint = Point(
                                sortX.Item(sortX.Length-1).highXYZ.x + notZero,
                                sortY.Item(sortY.Length-1).highXYZ.y + notZero,
                                sortZ.Item(sortZ.Length-1).highXYZ.z + notZero)
        lowPoint, highPoint

    // ----------------------------- getOuterBoundinBox TEST BEGIN -----------------------------

    let testgetOuterBoundinBox = getOuterBoundinBox qsortTestDataInput

    // ----------------------------- getOuterBoundinBox TEST END -----------------------------

    let buildBVHTree (xs:list<BBox>) = 
        if xs.Length = 0 then failwith "Unable to build BVH Tree, lists is empty."
        
        let firstAxisSplit = 0 // x=0, y=1, z=2
        let sortList = sortListByAxis xs firstAxisSplit
        let lowPoint, highPoint = getOuterBoundinBox(xs)

        let find =
            let mutable value = highPoint.X - lowPoint.X
            let y = highPoint.Y - lowPoint.Y
            let z = highPoint.Z - lowPoint.Z

            if value < y then value <- y
            else if value < z then value <- z
            value
        find
         

        //let rec innerBuild sortList axis = 
            
// ----------------------------- getOuterBoundinBox TEST BEGIN -----------------------------

    let testBuildBVHTree = buildBVHTree qsortTestDataInput

// ----------------------------- getOuterBoundinBox TEST END -----------------------------

        

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
        