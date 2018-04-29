﻿namespace Acceleration

open Tracer.Basics

module KD_tree = 

    type ShapeBBox (maxXYZ:Point, minXYZ:Point, shape:int) =
        member this.maxXYZ = maxXYZ
        member this.minXYZ = minXYZ
        member this.shape = shape
        override this.ToString() =
            "ShapeBox(Max: "+maxXYZ.ToString()+", Min: "+minXYZ.ToString()+", shape: "+shape.ToString()+")"
        override this.GetHashCode() =
            hash (maxXYZ, minXYZ, shape)
        override this.Equals(x) = 
            match x with
            | :? ShapeBBox as box -> this.maxXYZ = box.maxXYZ && 
                                     this.minXYZ = box.minXYZ && 
                                     this.shape = box.shape
            | _ -> false

    type BBox (maxXYZ:Point, minXYZ:Point) =
        member this.maxXYZ = maxXYZ
        member this.minXYZ = minXYZ
        override this.ToString() =
            "BBox(Max: "+maxXYZ.ToString()+", Min: "+minXYZ.ToString()+")"
        override this.GetHashCode() =
            hash (maxXYZ, minXYZ)
        override this.Equals(x) = 
            match x with
            | :? BBox as box -> this.maxXYZ = box.maxXYZ && 
                                     this.minXYZ = box.minXYZ
            | _ -> false

    type KDTree2 = Leaf of BBox * ShapeBBox list
                 | Node of int * float * BBox * KDTree2 * KDTree2

    [<AllowNullLiteral>]
    type KDTree(axis:int, splitValue:float, bBox:BBox, left:KDTree, right:KDTree, shapeList:list<ShapeBBox>, isLeaf:bool) = 
        member this.axis = axis
        member this.splitValue = splitValue
        member this.bBox = bBox
        member this.left = left
        member this.right = right
        member this.shapeList = shapeList
        member this.isLeaf = isLeaf
        new(axis:int, splitValue:float, bBox:BBox, left:KDTree, right:KDTree) = 
            KDTree(axis, splitValue, bBox, left, right, [], false)
        new(bBox:BBox, shapes:list<ShapeBBox>) = 
            KDTree(3, -infinity, bBox, null, null, shapes, true)
        override this.ToString() =
            
            if right = null && left = null then "Leaf: (Axis: "+axis.ToString()+
                                                      ", SplitValue: "+splitValue.ToString()+
                                                      ", "+bBox.ToString()+
                                                      ", ShapeBoxes: "+shapeList.ToString()+
                                                      ")"
            else if right = null then "Node: (Axis: "+axis.ToString()+
                                          ", SplitValue: "+splitValue.ToString()+
                                          ", "+bBox.ToString()+
                                          ", (Left: "+left.ToString()+
                                          "), ShapeBoxes: "+shapeList.ToString()+
                                          ")"
            else if left = null then "Node: (Axis: "+axis.ToString()+
                                          ", SplitValue: "+splitValue.ToString()+
                                          ", "+bBox.ToString()+
                                          ", (Right: "+right.ToString()+
                                          "), ShapeBoxes: "+shapeList.ToString()+
                                          ")"
            else "Node: (Axis: "+axis.ToString()+
                                          ", SplitValue: "+splitValue.ToString()+
                                          ", "+bBox.ToString()+
                                          ", (Left: "+left.ToString()+
                                          "), (Right: "+right.ToString()+
                                          "), ShapeBoxes: "+shapeList.ToString()+
                                          ")"
        override this.GetHashCode() =
            hash (axis, splitValue, bBox, left, right, shapeList)
        override this.Equals(x) =
            match x with
            | :? KDTree as tree -> this.axis = tree.axis && 
                                   this.bBox = tree.bBox && 
                                   this.left = tree.left && 
                                   this.right = tree.right && 
                                   this.splitValue = tree.splitValue &&
                                   this.shapeList = tree.shapeList
                                   
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
                | 2 -> let filterSmall = fun (e:ShapeBBox) -> e.maxXYZ.Z <= x.maxXYZ.Z
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
                    | 2 -> if x.maxXYZ.Z > max && x.minXYZ.Z < min then find xs x.maxXYZ.Z x.minXYZ.Z axis
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
                    if List.length boxes <= 1 then new KDTree(BBox(KDMaxXYZ, KDMinXYZ), boxes)
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
                    else if List.length newFirst = List.length oldBoxes && List.length newSecond = List.length oldBoxes then new KDTree(BBox(KDMaxXYZ, KDMinXYZ),oldBoxes)
                    else if List.length newFirst = List.length oldBoxes then new KDTree(axis, splitValue, BBox(KDMaxXYZ, KDMinXYZ), buildKDTree(newSecond), new KDTree(BBox(KDMaxXYZ, KDMinXYZ),newFirst))
                    else if List.length newSecond = List.length oldBoxes then new KDTree(axis, splitValue, BBox(KDMaxXYZ, KDMinXYZ), buildKDTree(newFirst), new KDTree(BBox(KDMaxXYZ, KDMinXYZ),newSecond))
                    else new KDTree(axis, splitValue, BBox(KDMaxXYZ, KDMinXYZ), buildKDTree(newSecond), buildKDTree(newFirst))
                if axis <= 2 then buildNodeX boxes axis
                else new KDTree(BBox(KDMaxXYZ, KDMinXYZ),boxes)
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




    let rec searchKDTree ((tree:KDTree), (ray:Ray), (t:float), (t':float), (shapes:array<Shape>)) = 
        if tree.isLeaf then let searchKDLeaf (tree:KDTree) (ray:Ray) (t':float) (shapes:array<Shape>) = 
                                let shape = closestHit tree.shapeList ray shapes
                                match shape with 
                                | Some shape -> if (shape.hitFunction ray).Time < t' then Some (shape.hitFunction ray)
                                                else None
                                | None -> None
                            searchKDLeaf tree ray t' shapes
        else let rec searchKDNode (tree:KDTree) (ray:Ray) (t:float) (t':float) (shapes:array<Shape>) = 
                 let a = tree.axis
                 if (findRayDirectionFromA a ray) = 0. then
                     if (findRayOriginFromA a ray) <= tree.splitValue then searchKDTree(tree.left, ray, t, t', shapes)
                     else searchKDTree(tree.right, ray, t, t', shapes)
                 else
                     let tHit = (tree.splitValue - (findRayOriginFromA a ray)) / (findRayDirectionFromA a ray)
                     let (first, second) = order((findRayDirectionFromA a ray), tree.left, tree.right)
                     if tHit <= t || tHit <= 0. then searchKDTree(second, ray, t, t', shapes)
                     else if tHit >= t' then searchKDTree(first, ray, t, t', shapes)
                     else let value = searchKDTree(first, ray, t, tHit, shapes)
                          match value with
                          | Some hitpoint -> value
                          | None -> searchKDTree(second, ray, tHit, t', shapes)
             searchKDNode tree ray t t' shapes
                            

    let traverseKDTree (tree:KDTree) (ray:Ray) (shapes:array<Shape>) = 
        let value = intersect tree.bBox ray
        match value with
        | Some (t, t') -> searchKDTree (tree, ray, t, t', shapes)
        | None -> None