namespace Tracer.Basics

module KD_tree = 

    type ShapeBBox (highPoint:Point, lowPoint:Point, shape:int) =
        member this.highPoint = highPoint
        member this.lowPoint = lowPoint
        member this.shape = shape
        override this.ToString() =
            "ShapeBox(Max: "+highPoint.ToString()+", Min: "+lowPoint.ToString()+", shape: "+shape.ToString()+")"
        override this.GetHashCode() =
            hash (highPoint, lowPoint, shape)
        override this.Equals(x) = 
            match x with
            | :? ShapeBBox as box -> this.highPoint = box.highPoint && 
                                     this.lowPoint = box.lowPoint && 
                                     this.shape = box.shape
            | _ -> false


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
    

    let rec qsort (xs:list<ShapeBBox>) axis =
        match xs with
        | [] -> []
        | x :: xs -> 
            let small, large = 
                match axis with 
                | 0 -> let filterSmall = fun (e:ShapeBBox) -> e.highPoint.X <= x.highPoint.X
                       let filterLarger = fun (e:ShapeBBox) -> e.highPoint.X >  x.highPoint.X
                       filterSmall, filterLarger
                | 1 -> let filterSmall = fun (e:ShapeBBox) -> e.highPoint.Y <= x.highPoint.Y
                       let filterLarger = fun (e:ShapeBBox) -> e.highPoint.Y >  x.highPoint.Y
                       filterSmall, filterLarger
                | 2 -> let filterSmall = fun (e:ShapeBBox) -> e.highPoint.Z <= x.highPoint.Z
                       let filterLarger = fun (e:ShapeBBox) -> e.highPoint.Z >  x.highPoint.Z
                       filterSmall, filterLarger
            let smaller = qsort (xs |> List.filter(small)) axis
            let larger  = qsort (xs |> List.filter(large)) axis
            smaller @ [x] @ larger


    let findMaxMin (xs:list<ShapeBBox>) axis = 
        match xs with
        | []    -> (infinity, infinity)
        | x::xs -> 
            let rec find (xs:list<ShapeBBox>) (max:float) (min:float) (axis:int) =
                match xs with
                | []    -> (max, min)
                | x::xs -> 
                    match axis with 
                    | 0 -> if x.highPoint.X > max && x.lowPoint.X < min then find xs x.highPoint.X x.lowPoint.X axis
                           else if x.highPoint.X > max then find xs x.highPoint.X min axis
                           else if x.lowPoint.X < min then find xs max x.lowPoint.X axis
                           else find xs max min axis
                    | 1 -> if x.highPoint.Y > max && x.lowPoint.Y < min then find xs x.highPoint.Y x.lowPoint.Y axis
                           else if x.highPoint.Y > max then find xs x.highPoint.Y min axis
                           else if x.lowPoint.Y < min then find xs max x.lowPoint.Y axis
                           else find xs max min axis
                    | 2 -> if x.highPoint.Z > max && x.lowPoint.Z < min then find xs x.highPoint.Z x.lowPoint.Z axis
                           else if x.highPoint.Z > max then find xs x.highPoint.Z min axis
                           else if x.lowPoint.Z < min then find xs max x.lowPoint.Z axis
                           else find xs max min axis
            find xs x.highPoint.X x.lowPoint.X axis


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
        | 2 -> point.Z


    let rec createKDTreeFromList (boxes:list<ShapeBBox>) = 
        match boxes with
        | []    -> failwith "There are no shapes to build a tree with!"
        | boxes -> 
            let (MaxX, MinX) = findMaxMin boxes 0
            let (MaxY, MinY) = findMaxMin boxes 1
            let (MaxZ, MinZ) = findMaxMin boxes 2
            printfn "Max: %A, Min: %A" MaxX MinX
            let KDMaxXYZ = Point(MaxX, MaxY, MaxZ)
            let KDMinXYZ = Point(MinX, MinY, MinZ)
            //if boxes.Length < 10 then KDTree(BBox(KDMinXYZ, KDMaxXYZ), boxes) //Check for less than 10 shapes. If that is the case, no KD-tree will be built
            //else
            let XDistance = MaxX - MinX
            let YDistance = MaxY - MinY
            let ZDistance = MaxZ - MinZ
            let rec buildNode boxes (xVisited, yVisited, zVisited, axis) = 
                let buildNodeWithSpecifiedAxis boxes axis = 
                    if List.length boxes <= 1 then new KDTree(BBox(KDMinXYZ, KDMaxXYZ), boxes)
                    else
                    let oldBoxes = boxes
                    let SortedBoxes = qsort boxes axis
                    let length = List.length SortedBoxes
                    let (first, second) = List.splitAt ((length/2)) SortedBoxes
                    let splitValue = findPointAxis (first.[(List.length first)-1].highPoint) axis
                    let newSecond = second
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    let newFirst = first @ (List.filter(fun n -> (findPointAxis(n.lowPoint) axis) < splitValue) second)
                    if ((float(List.length newFirst))-firstlength) > (((secondLength*60.))/100.) then buildNode oldBoxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                    else if List.length newFirst = List.length oldBoxes && List.length newSecond = List.length oldBoxes then new KDTree(BBox(KDMinXYZ, KDMaxXYZ),oldBoxes)
                    else if List.length newFirst = List.length oldBoxes then new KDTree(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), new KDTree(BBox(KDMinXYZ, KDMaxXYZ),newFirst), createKDTreeFromList(newSecond))
                    else if List.length newSecond = List.length oldBoxes then new KDTree(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), createKDTreeFromList(newFirst), new KDTree(BBox(KDMinXYZ, KDMaxXYZ),newSecond))
                    else new KDTree(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), createKDTreeFromList(newFirst), createKDTreeFromList(newSecond))
                if axis <= 2 then buildNodeWithSpecifiedAxis boxes axis
                else new KDTree(BBox(KDMinXYZ, KDMaxXYZ),boxes)
            buildNode boxes (findNextAxis (XDistance, YDistance, ZDistance, false, false, false))

    let buildKDTree (shapes:array<Shape>) = 
        let shapeBoxList = Array.zeroCreate(shapes.Length)
        for i in 0..(shapes.Length-1) do
            let id = i
            let shape = shapes.[i]
            let newShapeBox = ShapeBBox((shape.getBoundingBox ()).highPoint, (shape.getBoundingBox ()).lowPoint, id)
            shapeBoxList.[i] <- newShapeBox
        printfn "KD-build Initialized..."
        createKDTreeFromList (shapeBoxList |> Array.toList)

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


    let closestHit (shapeBoxes:list<ShapeBBox>) (ray:Ray) (shapes:array<Shape>) =
        // Get all hit points
        let pointsThatHit = 
            [for s in shapeBoxes do yield (shapes.[s.shape].hitFunction ray )]
                |> List.filter (fun (hp:HitPoint) -> hp.DidHit)
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            None
        else
            // If the ray hit, then return the first hit point
            Some (pointsThatHit |> List.minBy (fun (hp) -> hp.Time))
    
    let order (d:float, left:KDTree, right:KDTree) =
        if d > 0. then (left, right)
        else (right, left)




    let rec searchKDTree ((tree:KDTree), (ray:Ray), (t:float), (t':float), (shapes:array<Shape>)) = 
        if tree.isLeaf then 
            let searchKDLeaf (tree:KDTree) (ray:Ray) (t':float) (shapes:array<Shape>) = 
                let option = closestHit tree.shapeList ray shapes
                match option with 
                | Some (hit) -> if hit.Time < t' then 
                                                 hit
                                       else 
                                           HitPoint (ray)
                | None -> HitPoint (ray)
            searchKDLeaf tree ray t' shapes
        else 
             let searchKDNode (tree:KDTree) (ray:Ray) (t:float) (t':float) (shapes:array<Shape>) = 
                 let a = tree.axis
                 if (findRayDirectionFromA a ray) = 0.0 then
                     if (findRayOriginFromA a ray) <= tree.splitValue then 
                        searchKDTree(tree.left, ray, t, t', shapes)
                     else 
                        searchKDTree(tree.right, ray, t, t', shapes)
                 else
                     let tHit = (tree.splitValue - (findRayOriginFromA a ray)) / (findRayDirectionFromA a ray)
                     let (first, second) = order((findRayDirectionFromA a ray), tree.left, tree.right)
                     if tHit <= t || tHit <= 0.0 then 
                        searchKDTree(second, ray, t, t', shapes)
                     else 
                        if tHit >= t' then 
                            searchKDTree(first, ray, t, t', shapes)
                        else 
                            let hitpoint = searchKDTree(first, ray, t, tHit, shapes)
                            let returnPoint = hitpoint
                            if hitpoint.DidHit then 
                                returnPoint
                            else 
                                searchKDTree(second, ray, tHit, t', shapes)
             searchKDNode tree ray t t' shapes
                            

    let traverseKDTree (tree:KDTree) (ray:Ray) (shapes:array<Shape>) = 
        let value = tree.bBox.intersect ray
        match value with
        | Some (t, t') -> searchKDTree (tree, ray, t, t', shapes)
        | None -> HitPoint (ray)