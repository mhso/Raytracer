namespace Acceleration

open Tracer.Basics

module KD_tree = 

    type ShapeBBox (maxXYZ:Point, minXYZ:Point, shape:int) =
        member this.maxXYZ = maxXYZ
        member this.minXYZ = minXYZ
        member this.shape = shape
        override this.GetHashCode() =
            hash (maxXYZ, minXYZ, shape)
        override this.Equals(x) = 
            match x with
            | :? ShapeBBox as box -> this.maxXYZ.Equals(box.maxXYZ) && 
                                     this.minXYZ.Equals(box.minXYZ) && 
                                     this.shape.Equals(box.shape)
            | _ -> false

    type BBox (maxXYZ:Point, minXYZ:Point) =
        member this.maxXYZ = maxXYZ
        member this.minXYZ = minXYZ
        override this.GetHashCode() =
            hash (maxXYZ, minXYZ)
        override this.Equals(x) = 
            match x with
            | :? ShapeBBox as box -> this.maxXYZ.Equals(box.maxXYZ) && 
                                     this.minXYZ.Equals(box.minXYZ)
            | _ -> false

    type KDTree = Leaf of BBox * ShapeBBox list
                | Node of int * float * BBox * KDTree * KDTree

    [<AllowNullLiteral>]
    type KDTree2(axis:int, splitValue:float, bBox:BBox, left:KDTree2, right:KDTree2, shapeList:list<ShapeBBox>) = 
        member this.axis = axis
        member this.splitValue = splitValue
        member this.bBox = bBox
        member this.left = left
        member this.right = right
        member this.shapeList = shapeList
        new(axis:int, splitValue:float, bBox:BBox, left:KDTree2, right:KDTree2) = 
            KDTree2(axis, splitValue, bBox, left, right, [])
        new(bBox:BBox, shapes:list<ShapeBBox>) = 
            KDTree2(3, -infinity, bBox, null, null, shapes)
        override this.GetHashCode() =
            hash (axis, splitValue, bBox, left, right, shapeList)
        override this.Equals(x) =
            match x with
            | :? KDTree2 as tree -> tree.axis.Equals(axis) && 
                                    tree.bBox.Equals(box) && 
                                    tree.left.Equals(left) && 
                                    tree.right.Equals(right) && 
                                    tree.splitValue.Equals(splitValue)
            | _ -> false
    



    //Temporary Intersect-function. Use Alexanders when available.
    let intersect (box:BBox)(r:Ray) = 
        let tx = if r.GetDirection.X >= 0.0 then (box.minXYZ.X - r.GetOrigin.X)/r.GetDirection.X else (box.maxXYZ.X - r.GetOrigin.X)/r.GetDirection.X
        let tx' = if r.GetDirection.X >= 0.0 then (box.maxXYZ.X - r.GetOrigin.X)/r.GetDirection.X else (box.minXYZ.X - r.GetOrigin.X)/r.GetDirection.X
        let ty = if r.GetDirection.Y >= 0.0 then (box.minXYZ.Y - r.GetOrigin.Y)/r.GetDirection.Y else (box.maxXYZ.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let ty' = if r.GetDirection.Y >= 0.0 then (box.maxXYZ.Y - r.GetOrigin.Y)/r.GetDirection.Y else (box.minXYZ.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let tz = if r.GetDirection.Z >= 0.0 then (box.minXYZ.Z - r.GetOrigin.Z)/r.GetDirection.Z else (box.maxXYZ.Z - r.GetOrigin.Z)/r.GetDirection.Z
        let tz' = if r.GetDirection.Z >= 0.0 then (box.maxXYZ.Z - r.GetOrigin.Z)/r.GetDirection.Z else (box.minXYZ.Z - r.GetOrigin.Z)/r.GetDirection.Z
        

        let t = max tx (max ty tz)

        let t' = min tx' (min ty' tz')

        if t < t' && t' > 0.0 then Some(t, t')
        else None

    let rec qsort (xs:list<ShapeBBox>) axis =
        match xs with
        | [] -> []
        | x :: xs -> 
            let small, large = 
                match axis with 
                | 0 -> let filterSmall = fun (e:ShapeBBox) -> e.maxXYZ.X <= x.maxXYZ.X
                       let filterLarger = fun (e:ShapeBBox) -> e.maxXYZ.X >  x.maxXYZ.X
                       filterSmall, filterLarger
                | 1 -> let filterSmall = fun (e:ShapeBBox) -> e.maxXYZ.Y <= x.maxXYZ.Y
                       let filterLarger = fun (e:ShapeBBox) -> e.maxXYZ.Y >  x.maxXYZ.Y
                       filterSmall, filterLarger
                | _ -> let filterSmall = fun (e:ShapeBBox) -> e.maxXYZ.Z <= x.maxXYZ.Z
                       let filterLarger = fun (e:ShapeBBox) -> e.maxXYZ.Z >  x.maxXYZ.Z
                       filterSmall, filterLarger
            let smaller = qsort (xs |> List.filter(small)) axis
            let larger  = qsort (xs |> List.filter(large)) axis
            smaller @ [x] @ larger


    //Not working... I think!!!
    let findMaxMin (xs:list<ShapeBBox>) axis = 
        match xs with
        | []    -> (infinity, infinity)
        | x::xs -> 
            let rec find (xs:list<ShapeBBox>) (max:float) (min:float) (axis:int) =
                match xs with
                | []    -> (max, min)
                | x::xs -> 
                    match axis with 
                    | 0 -> if x.maxXYZ.X > max && x.minXYZ.X < min then find xs x.maxXYZ.X x.minXYZ.X axis
                           else if x.maxXYZ.X > max then find xs x.maxXYZ.X min axis
                           else if x.minXYZ.X < min then find xs max x.minXYZ.X axis
                           else find xs max min axis
                    | 1 -> if x.maxXYZ.Y > max && x.minXYZ.Y < min then find xs x.maxXYZ.Y x.minXYZ.Y axis
                           else if x.maxXYZ.Y > max then find xs x.maxXYZ.Y min axis
                           else if x.minXYZ.Y < min then find xs max x.minXYZ.Y axis
                           else find xs max min axis
                    | _ -> if x.maxXYZ.Z > max && x.minXYZ.Z < min then find xs x.maxXYZ.Z x.minXYZ.Z axis
                           else if x.maxXYZ.Z > max then find xs x.maxXYZ.Z min axis
                           else if x.minXYZ.Z < min then find xs max x.minXYZ.Z axis
                           else find xs max min axis
            find xs x.maxXYZ.X x.minXYZ.X axis //Possible issue


    // Long ugly function with lots of if-statements to check which axis to split on.
    let findNextAxis (xDistance, yDistance, zDistance, xVisited, yVisited, zVisited) = 
        match (xVisited, yVisited, zVisited) with
        | (false, false, false) -> 
            if      xDistance >= yDistance && xDistance >= zDistance then (true, yVisited, zVisited, 0)
            else if yDistance > xDistance && yDistance >= zDistance then (xVisited, true, zVisited, 1)
            else    (xVisited, yVisited, true, 2)
        | (true, false, false)  -> 
            if      yDistance >= zDistance then (xVisited, true, zVisited, 1)
            else    (xVisited, yVisited, true, 2)
        | (false, true, false)  -> 
            if      xDistance >= zDistance then (true, yVisited, zVisited, 0)
            else    (xVisited, yVisited, true, 2)
        | (false, false, true)  -> 
            if      xDistance > yDistance then (true, yVisited, zVisited, 0)
            else    (xVisited, true, zVisited, 1)
        | (true, true, false)   -> (xVisited, yVisited, true, 2)
        | (false, true, true)   -> (true, yVisited, zVisited, 0)
        | (true, false, true)   -> (xVisited, true, zVisited, 1)
        | (true, true, true)    -> (xVisited, yVisited, zVisited, 3)

    let findPointAxis (point:Point) axis = 
        match axis with
        | 0 -> point.X
        | 1 -> point.Y
        | _ -> point.Z


    let rec buildKDTree (boxes:list<ShapeBBox>) = 
        printfn "KD Branch build start"
        //if (List.length boxes) < 10 then Leaf(boxes) //Check for less than 10 shapes. If that is the case, no KD-tree will be built
        //else
        match boxes with
        | []    -> failwith "There is no shapes to build a tree with!"
        | boxes -> 
            let newBoxesX = boxes        //All the 8 lines below is all set up for the longest-axis check
            let (MaxX, MinX) = findMaxMin newBoxesX 0
            let newBoxesY = boxes 
            let (MaxY, MinY) = findMaxMin newBoxesY 1
            let newBoxesZ = boxes
            let (MaxZ, MinZ) = findMaxMin newBoxesZ 2
            let KDMaxXYZ = Point(MaxX, MaxY, MaxZ)
            let KDMinXYZ = Point(MinX, MinY, MinZ)
            let XDistance = MaxX - MinX
            let YDistance = MaxY - MinY
            let ZDistance = MaxZ - MinZ
            let rec buildNode boxes (xVisited, yVisited, zVisited, axis) = 
                let buildNodeX boxes axis = 
                    printfn "Split %A" axis
                    if List.length boxes <= 1 then Leaf(BBox(KDMaxXYZ, KDMinXYZ), boxes)
                    else
                    let oldBoxes = boxes
                    let SortedBoxes = qsort boxes axis
                    let length = List.length SortedBoxes
                    let (first, second) = List.splitAt ((length/2)) SortedBoxes
                    let splitValue = findPointAxis (first.[(List.length first)-1].maxXYZ) axis
                    let newSecond = second
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    let newFirst = first @ (List.filter(fun n -> (findPointAxis(n.minXYZ) axis) < splitValue) second)
                    if ((float(List.length newFirst))-firstlength) > (((secondLength*60.))/100.) then buildNode oldBoxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                    else if List.length newFirst = List.length oldBoxes && List.length newSecond = List.length oldBoxes then Leaf(BBox(KDMaxXYZ, KDMinXYZ),oldBoxes)
                    else if List.length newFirst = List.length oldBoxes then Node(axis, splitValue, BBox(KDMaxXYZ, KDMinXYZ), buildKDTree(newSecond), Leaf(BBox(KDMaxXYZ, KDMinXYZ),newFirst))
                    else if List.length newSecond = List.length oldBoxes then Node(axis, splitValue, BBox(KDMaxXYZ, KDMinXYZ), buildKDTree(newFirst), Leaf(BBox(KDMaxXYZ, KDMinXYZ),newSecond))
                    else Node(axis, splitValue, BBox(KDMaxXYZ, KDMinXYZ), buildKDTree(newSecond), buildKDTree(newFirst))
                if axis <= 2 then buildNodeX boxes axis
                else Leaf(BBox(KDMaxXYZ, KDMinXYZ),boxes)
            buildNode boxes (findNextAxis (XDistance, YDistance, ZDistance, false, false, false))

    let findRayDirectionFromA (a:int) (r:Ray) =
        match a with
        | 0 -> r.GetDirection.X
        | 1 -> r.GetDirection.Y
        | 2 -> r.GetDirection.Z

    let findRayOriginFromA (a:int) (r:Ray) =
        match a with
        | 0 -> r.GetOrigin.X
        | 1 -> r.GetOrigin.Y
        | 2 -> r.GetOrigin.Z

    let closestHit (shapeBoxes:list<ShapeBBox>)(ray:Ray)(shapes:array<Shape>) =
        let mutable closestShape = None
        let mutable closestDist = infinity
        for shapeRef in shapeBoxes do
            let hit = shapes.[shapeRef.shape].hitFunction ray
            let dist = hit.Time
            if dist < closestDist then
                closestDist <- dist
                closestShape <- Some shapes.[shapeRef.shape]
        closestShape
    
    let order (d:float, left:KDTree, right:KDTree) =
        if d > 0. then (left, right)
        else (right, left)


    let rec searchKDTree ((node:KDTree), (ray:Ray), (t:float), (t':float), (shapes:array<Shape>)) = 
        match node with
        | Leaf(bBox, sBox)                      -> 
            let searchKDLeaf node (ray:Ray) (t':float) (shapes:array<Shape>) = 
                match node with
                | Leaf(box, shapeBoxes) -> 
                    let shape = closestHit shapeBoxes ray shapes
                    match shape with 
                    | Some shape -> if (shape.hitFunction ray).Time < t' then Some (shape.hitFunction ray)
                                    else None
                    | None -> None
                | Node(axis, splitV, box, left, right) -> failwith "There should not be a Node here!"
            searchKDLeaf node ray t' shapes
        | Node(axis, splitV, bBox, left, right) -> 
            let rec searchKDNode (node:KDTree) (ray:Ray) (t:float) (t':float) (shapes:array<Shape>) = 
                match node with
                | Node(axis,splitValue,box,left,right) ->
                        let a = axis
                        if (findRayDirectionFromA a ray) = 0. then
                            if (findRayOriginFromA a ray) <= splitValue then searchKDTree(left, ray, t, t', shapes)
                            else searchKDTree(right, ray, t, t', shapes)
                        else
                            let tHit = (splitValue - (findRayOriginFromA a ray)) / (findRayDirectionFromA a ray)
                            let (first, second) = order((findRayDirectionFromA a ray), left, right)
                            if tHit <= t || tHit <= 0. then searchKDTree(second, ray, t, t', shapes)
                            else if tHit >= t' then searchKDTree(first, ray, t, t', shapes)
                            else let value = searchKDTree(first, ray, t, tHit, shapes)
                                 match value with
                                 | Some hitpoint -> value
                                 | None -> searchKDTree(second, ray, tHit, t', shapes)
                | Leaf(box, shapesBoxes) -> failwith "A Leaf should not be present here!"
            searchKDNode node ray t t' shapes

    let traverseKDTree (tree:KDTree) (ray:Ray) (shapes:array<Shape>) = 
        match tree with
        | Node(s, split, bbox, left, right) as n -> let value = intersect bbox ray 
                                                    match value with 
                                                    | Some (t, t') -> searchKDTree (n, ray, t, t', shapes)
                                                    | None -> None
        | Leaf(bbox, boxes) as L                 -> let value = intersect bbox ray 
                                                    match value with 
                                                    | Some (t, t') -> searchKDTree (L, ray, t, t', shapes)
                                                    | None -> None