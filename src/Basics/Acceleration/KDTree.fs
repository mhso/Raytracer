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

    type KDTree = Node of int * float * BBox * KDTree * KDTree
                 | Leaf of BBox * ShapeBBox list


    let rec quickselect k (list:list<ShapeBBox>) axis = 
        match list with
        | [] -> failwith "Cannot take largest element of empty list."
        | [a] -> a
        | x::xs ->
            match axis with
            | 0 ->  let (ys, zs) = List.partition (fun (arg:ShapeBBox) -> arg.highPoint.X < x.highPoint.X) xs
                    let l = List.length ys
                    if k < l then quickselect k ys axis
                    elif k > l then quickselect (k-l-1) zs axis
                    else x
            | 1 ->  let (ys, zs) = List.partition (fun (arg:ShapeBBox) -> arg.highPoint.Y < x.highPoint.Y) xs
                    let l = List.length ys
                    if k < l then quickselect k ys axis
                    elif k > l then quickselect (k-l-1) zs axis
                    else x
            | 2 ->  let (ys, zs) = List.partition (fun (arg:ShapeBBox) -> arg.highPoint.Z < x.highPoint.Z) xs
                    let l = List.length ys
                    if k < l then quickselect k ys axis
                    elif k > l then quickselect (k-l-1) zs axis
                    else x


    let findMaxMin (xs:list<ShapeBBox>) axis = 
        match xs with
        | [] -> (0., 0.)
        | _  -> 
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
            find xs 0. 0. axis


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

    let partitionAfterSelect2 (boxes:list<ShapeBBox>) (splitV:float) axis =
        let rec inner left right (tosort:list<ShapeBBox>) axis = 
            match tosort with
              | [] -> left,right
              | c::cr -> 
                match axis with
                | 0 -> let l = if c.lowPoint.X < splitV then c::left
                               else left
                       let r = if c.highPoint.X > splitV then c::right
                               else right
                       inner l r cr axis
                | 1 -> let l = if c.lowPoint.Y < splitV then c::left
                               else left
                       let r = if c.highPoint.Y > splitV then c::right
                               else right
                       inner l r cr axis
                | 2 -> let l = if c.lowPoint.Z < splitV then c::left
                               else left
                       let r = if c.highPoint.Z > splitV then c::right
                               else right
                       inner l r cr axis
        inner [] [] boxes axis

    let partitionAfterSelect (boxes:list<ShapeBBox>) (splitV:float) axis = 
        match axis with
        | 0 -> let (f, s) = boxes |> List.partition (fun arg -> arg.highPoint.X < splitV)
               (f, s)
        | 1 -> let (f, s) = boxes |> List.partition (fun arg -> arg.highPoint.Y < splitV)
               (f, s)
        | 2 -> let (f, s) = boxes |> List.partition (fun arg -> arg.highPoint.Z < splitV)
               (f, s)

    let mutable isFirstNode = true
    let mutable right = 0
    let mutable left = 0

    let findSplitValue (boxes:list<ShapeBBox>) axis = 
        let boxesLength = float boxes.Length
        if boxesLength = 0. then 0.
        else
        let rec inner acc (boxes:list<ShapeBBox>) axis = 
            match boxes with
            | [] -> acc/boxesLength
            | x::xs -> 
                match axis with
                | 0 -> inner(acc+((x.highPoint.X+x.lowPoint.X)/2.)) xs axis
                | 1 -> inner(acc+((x.highPoint.Y+x.lowPoint.Y)/2.)) xs axis
                | 2 -> inner(acc+((x.highPoint.Z+x.lowPoint.Z)/2.)) xs axis
        inner 0.0 boxes axis

    let mutable avgLeafSize = 0.

    let mutable totalLeafs = 0

    let mutable totalLeafSize = 0

    let mutable maxLeafSize = 0

    let rec createKDTreeFromList (boxes:list<ShapeBBox>) = 
        match boxes with
        | []    -> failwith "There are no shapes to build a tree with!"
        | boxes -> 
            let (MaxX, MinX) = findMaxMin boxes 0
            let (MaxY, MinY) = findMaxMin boxes 1
            let (MaxZ, MinZ) = findMaxMin boxes 2
            //printfn "MinMax found"
            let KDMaxXYZ = Point(MaxX, MaxY, MaxZ)
            let KDMinXYZ = Point(MinX, MinY, MinZ)
            let XDistance = MaxX - MinX
            let YDistance = MaxY - MinY
            let ZDistance = MaxZ - MinZ
            let rec buildNode boxes (xVisited, yVisited, zVisited, axis) = 
                //printfn "buildNode started"
                let buildNodeWithSpecifiedAxis boxes axis = 
                    //printfn "buoldnode with axis"
                    if List.length boxes <= 1 then if boxes.Length > maxLeafSize then maxLeafSize <- boxes.Length
                                                   totalLeafs <- totalLeafs+1
                                                   totalLeafSize <- totalLeafSize+boxes.Length
                                                   Leaf(BBox(KDMinXYZ, KDMaxXYZ), boxes)
                    else
                    let splitV = findSplitValue boxes axis
                    //printfn "quickkSelect"
                    let (first, second) = partitionAfterSelect2 boxes splitV axis
                    //printfn "%A" first
                    //printfn "%A" second
                    //printfn "%A" splitV
                    //printfn "Original Length%A" boxes.Length
                    //printfn "axis: %i" axis
                    //printfn "%A %A" first.Length second.Length
                    //printfn "partition"
                    let splitValue = splitV
                    //printfn "splitvalue"
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    //printfn "list filter"
                    //if isFirstNode = true then isFirstNode <- false
                    //                           right <- newSecond.Length
                    //                           left <- newFirst.Length
                    //                           printfn "left: %A, right: %A" left right
                    if (firstlength+secondLength)/(float(boxes.Length)) > 1.3  then buildNode boxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                    else if List.length first = List.length boxes && List.length second = List.length boxes then 
                                                                                                                 if boxes.Length > maxLeafSize then maxLeafSize <- boxes.Length
                                                                                                                 totalLeafs <- totalLeafs+1
                                                                                                                 totalLeafSize <- totalLeafSize+boxes.Length
                                                                                                                 Leaf(BBox(KDMinXYZ, KDMaxXYZ),boxes)
                    else if List.length first = List.length boxes then 
                                                                       if boxes.Length > maxLeafSize then maxLeafSize <- first.Length
                                                                       totalLeafs <- totalLeafs+1
                                                                       totalLeafSize <- totalLeafSize+first.Length
                                                                       Node(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), Leaf(BBox(KDMinXYZ, KDMaxXYZ),first), createKDTreeFromList(second))
                    else if List.length second = List.length boxes then 
                                                                        if boxes.Length > maxLeafSize then maxLeafSize <- first.Length
                                                                        totalLeafs <- totalLeafs+1
                                                                        totalLeafSize <- totalLeafSize+first.Length
                                                                        Node(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), createKDTreeFromList(first), Leaf(BBox(KDMinXYZ, KDMaxXYZ),second))
                    else Node(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), createKDTreeFromList(first), createKDTreeFromList(second))
                if axis <= 2 then buildNodeWithSpecifiedAxis boxes axis
                else 
                     if boxes.Length > maxLeafSize then maxLeafSize <- boxes.Length
                     totalLeafs <- totalLeafs+1
                     totalLeafSize <- totalLeafSize+boxes.Length
                     Leaf(BBox(KDMinXYZ, KDMaxXYZ),boxes)
            buildNode boxes (findNextAxis (XDistance, YDistance, ZDistance, false, false, false))

    let buildKDTree (shapes:array<Shape>) = 
        let shapeBoxArray = Array.zeroCreate(shapes.Length)
        for i in 0..(shapes.Length-1) do
            let id = i
            let shape = shapes.[i]
            let newShapeBox = ShapeBBox((shape.getBoundingBox ()).highPoint, (shape.getBoundingBox ()).lowPoint, id)
            shapeBoxArray.[i] <- newShapeBox
        let ShapeBoxList = (shapeBoxArray |> Array.toList)
        //printfn "KD-build Initialized..."
        if shapeBoxArray.Length < 11 then 
            let (MaxX, MinX) = findMaxMin ShapeBoxList 0
            let (MaxY, MinY) = findMaxMin ShapeBoxList 1
            let (MaxZ, MinZ) = findMaxMin ShapeBoxList 2
            let KDMaxXYZ = Point(MaxX, MaxY, MaxZ)
            let KDMinXYZ = Point(MinX, MinY, MinZ)
            Leaf(BBox(KDMinXYZ, KDMaxXYZ), ShapeBoxList) //Check for 10 shaper or less. If that is the case, no KD-tree will be built
        else
            printfn "KD-Build initialized..."
            printfn "Total shape count: %A" ShapeBoxList.Length
            let kdTree = createKDTreeFromList ShapeBoxList
            printfn "MaxLeafSize: %A, Total Leafs: %A" maxLeafSize totalLeafs
            printfn "Total Leaf Size: %A, Avg Leaf Size: %A" totalLeafSize (totalLeafSize/totalLeafs)
            kdTree

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

    let getFirstHitPoint (shapeBoxes:list<ShapeBBox>) (shapes:array<Shape>) (ray:Ray) : HitPoint = 
      let rec findClosestHit (h:HitPoint) t' (shapeBoxes:list<ShapeBBox>) (shapes:array<Shape>) = 
        match shapeBoxes with
        | []    -> h
        | (s:ShapeBBox)::sl -> 
                   let hit = shapes.[s.shape].hitFunction ray
                   if hit.DidHit && hit.Time < t' then findClosestHit hit hit.Time sl shapes
                   else findClosestHit h t' sl shapes
      findClosestHit (HitPoint(ray)) infinity shapeBoxes shapes


    let closestHit (shapeBoxes:list<ShapeBBox>) (ray:Ray) (shapes:array<Shape>) =
        
        // Get all hit points
        //let pointsThatHit = 
        //    [for s in shapeBoxes do yield (let hitBox = shapes.[s.shape].getBoundingBox().intersect ray
        //                                   match hitBox with
        //                                   | Some(hit) -> shapes.[s.shape].hitFunction ray
        //                                   | None -> HitPoint(ray)
        //                                   )]
        //        |> List.filter (fun (hp:HitPoint) -> hp.DidHit)
        //let pointsThatHit = 
        //    [for s in shapeBoxes do yield (shapes.[s.shape].hitFunction ray )]
        //        |> List.filter (fun (hp:HitPoint) -> hp.DidHit)
        let hit = getFirstHitPoint shapeBoxes shapes ray
        // Check if the ray hit
        if hit.DidHit then
            // If not, return an empty hit point
            Some (hit)
        else
            // If the ray hit, then return the first hit point
            None
    
    let order (d:float, left:KDTree, right:KDTree) =
        if d > 0. then (left, right)
        else (right, left)


    let rec searchKDTree (tree:KDTree) (ray:Ray) (t:float) (t':float) (shapes:array<Shape>):HitPoint = 
        let SearchKDLeaf (tree:KDTree) (ray:Ray) (t':float) (shapes:array<Shape>) = 
            match tree with
            | Node(_) -> failwith "Should never be a node here..."
            | Leaf(bBox, shapeList) -> let option = closestHit shapeList ray shapes
                                       match option with
                                       | Some(hit) -> if hit.Time < t' then
                                                                       hit
                                                      else
                                                      HitPoint(ray)
                                       |None ->       HitPoint(ray)
        
        let searchKDNode (tree:KDTree) (ray:Ray) (t:float) (t':float) (shapes:array<Shape>) = 
            match tree with
            | Leaf(_) -> failwith "Should never be a leaf here..."
            | Node(axis, value, bBox, left, right) ->
                let a = axis
                if (findRayDirectionFromA a ray) = 0.0 then
                    if (findRayOriginFromA a ray) <= value then
                        searchKDTree left ray t t' shapes
                    else
                        searchKDTree right ray t t' shapes
                else
                    let tHit = (value - (findRayOriginFromA a ray)) / (findRayDirectionFromA a ray)
                    let (first, second) = order((findRayDirectionFromA a ray), left, right)
                    if tHit <= t || tHit <= 0.0 then
                        searchKDTree second ray t t' shapes
                    else
                        if tHit >= t' then
                            searchKDTree first ray t t' shapes
                        else
                            let hitPoint = searchKDTree first ray t tHit shapes
                            let returnPoint = hitPoint
                            if hitPoint.DidHit then
                                returnPoint
                            else
                                searchKDTree second ray tHit t' shapes
        match tree with 
        | Node(axis, value, bBox, left, right) -> searchKDNode tree ray t t' shapes
        | Leaf(bBox, shapeList)                -> SearchKDLeaf tree ray t' shapes
                            

    let traverseKDTree (tree:KDTree) (ray:Ray) (shapes:array<Shape>) = 
        match tree with
        | Node(axis, value, bBox, left, right) -> let intersect = bBox.intersect ray
                                                  match intersect with
                                                  | Some (t, t') -> searchKDTree tree ray t t' shapes
                                                  | None -> HitPoint (ray)
        | Leaf(bBox, shapeList)                -> let intersect = bBox.intersect ray
                                                  match intersect with
                                                  | Some (t, t') -> searchKDTree tree ray t t' shapes
                                                  | None -> HitPoint (ray)