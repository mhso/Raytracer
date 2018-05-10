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

    let partitionAfterSelect (boxes:list<ShapeBBox>) (splitBox:ShapeBBox) axis = 
        match axis with
        | 0 -> let (f, s) = boxes |> List.partition (fun arg -> arg.highPoint.X < splitBox.highPoint.X)
               ((f @ [splitBox]), s)
        | 1 -> let (f, s) = boxes |> List.partition (fun arg -> arg.highPoint.Y < splitBox.highPoint.Y)
               ((f @ [splitBox]), s)
        | 2 -> let (f, s) = boxes |> List.partition (fun arg -> arg.highPoint.Z < splitBox.highPoint.Z)
               ((f @ [splitBox]), s)


    let rec createKDTreeFromList (boxes:list<ShapeBBox>) = 
        match boxes with
        | []    -> failwith "There are no shapes to build a tree with!"
        | boxes -> 
            let (MaxX, MinX) = findMaxMin boxes 0
            let (MaxY, MinY) = findMaxMin boxes 1
            let (MaxZ, MinZ) = findMaxMin boxes 2
            let KDMaxXYZ = Point(MaxX, MaxY, MaxZ)
            let KDMinXYZ = Point(MinX, MinY, MinZ)
            let XDistance = MaxX - MinX
            let YDistance = MaxY - MinY
            let ZDistance = MaxZ - MinZ
            let rec buildNode boxes (xVisited, yVisited, zVisited, axis) = 
                let buildNodeWithSpecifiedAxis boxes axis = 
                    if List.length boxes <= 1 then Leaf(BBox(KDMinXYZ, KDMaxXYZ), boxes)
                    else
                    let splitBox = quickselect ((List.length boxes)/2) boxes axis
                    let (first, second) = partitionAfterSelect boxes splitBox axis
                    let splitValue = findPointAxis (first.[(List.length first)-1].highPoint) axis
                    let newSecond = second
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    let newFirst = first @ (List.filter(fun n -> (findPointAxis(n.lowPoint) axis) < splitValue) second)
                    if ((float(List.length newFirst))-firstlength) > (((secondLength*60.))/100.) then buildNode boxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                    else if List.length newFirst = List.length boxes && List.length newSecond = List.length boxes then Leaf(BBox(KDMinXYZ, KDMaxXYZ),boxes)
                    else if List.length newFirst = List.length boxes then Node(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), Leaf(BBox(KDMinXYZ, KDMaxXYZ),newFirst), createKDTreeFromList(newSecond))
                    else if List.length newSecond = List.length boxes then Node(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), createKDTreeFromList(newFirst), Leaf(BBox(KDMinXYZ, KDMaxXYZ),newSecond))
                    else Node(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), createKDTreeFromList(newFirst), createKDTreeFromList(newSecond))
                if axis <= 2 then buildNodeWithSpecifiedAxis boxes axis
                else Leaf(BBox(KDMinXYZ, KDMaxXYZ),boxes)
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
            createKDTreeFromList ShapeBoxList

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
        //let pointsThatHit = 
        //    [for s in shapeBoxes do yield (let hitBox = shapes.[s.shape].getBoundingBox().intersect ray
        //                                   match hitBox with
        //                                   | Some(hit) -> shapes.[s.shape].hitFunction ray
        //                                   | None -> HitPoint(ray)
        //                                   )]
        //        |> List.filter (fun (hp:HitPoint) -> hp.DidHit)
        let pointsThatHit = 
            [for s in shapeBoxes do yield (shapes.[s.shape].hitFunction ray )]
                |> List.filter (fun (hp:HitPoint) -> hp.DidHit)
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            None
        else
            let closestHit = pointsThatHit |> List.minBy (fun (hp) -> hp.Time)
            if closestHit.Material :? EmissiveMaterial then
                None
            else
            // If the ray hit, then return the first hit point
                Some (pointsThatHit |> List.minBy (fun (hp) -> hp.Time))
    
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