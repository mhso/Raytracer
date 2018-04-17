namespace Tracer
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

    let rec qsort (xs:list<BBox>) (axis:int) =
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

          let smaller = qsort (xs |> List.filter(small)) axis
          let larger  = qsort (xs |> List.filter(large)) axis
          smaller @ [x] @ larger
    
    // ----------------------------- qsort TEST BEGIN -----------------------------
    let box1Test = {  lowXYZ = {x=1.; y=0.6; z=1.};
                      highXYZ = {x=6.5; y=9.; z=8.9};
                      shape = S(5.0) }
    let box2Test = {  lowXYZ = {x=7.; y=3.; z=8.4};
                      highXYZ = {x=12.; y=7.; z=16.6};
                      shape = S(4.0) }
    let box3Test = {  lowXYZ = {x=8.; y=10.; z=8.9};
                      highXYZ = {x=11.4; y=13.5; z=15.7};
                      shape = S(3.0) }

    let qsortTestDataInput = [box1Test; box2Test; box3Test]

    //let qsortTestX = qsort qsortTestDataInput 0
    //let qsortTestY = qsort qsortTestDataInput 1
    //let qsortTestZ = qsort qsortTestDataInput 2
    // ----------------------------- qsort TEST END -----------------------------

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
        let sortX  = qsort xs 0
        let sortY  = qsort xs 1
        let sortZ  = qsort xs 2

        let lowPoint = Point(
                                sortX.Head.lowXYZ.x,
                                sortY.Head.lowXYZ.y,
                                sortZ.Head.lowXYZ.z)
        let highPoint = Point(
                                sortX.Item(sortX.Length-1).highXYZ.x,
                                sortY.Item(sortY.Length-1).highXYZ.y,
                                sortZ.Item(sortZ.Length-1).highXYZ.z)
        lowPoint, highPoint

    // ----------------------------- getOuterBoundinBox TEST BEGIN -----------------------------

    let testgetOuterBoundinBox = getOuterBoundinBox qsortTestDataInput

    // ----------------------------- getOuterBoundinBox TEST END -----------------------------

    // let buildBVHTree (xs:list<BBox>) = 
        
        

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
        