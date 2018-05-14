namespace Tracer.Basics

module KD_tree = 

    type ShapeBBox (box:BBox, shape:int) =
        member this.box = box
        member this.shape = shape

    type KDTree = Node of int * float * BBox * KDTree * KDTree
                | Leaf of BBox * ShapeBBox list

    let timer = new System.Diagnostics.Stopwatch()


    let findMaxMin (xs:list<ShapeBBox>) = 
        match xs with
        | [] -> Point(0., 0., 0.), Point(0., 0., 0.)
        | _  -> 
            let rec find (maxX:float) (minX:float) (maxY:float) (minY:float) (maxZ:float) (minZ:float) (xs:list<ShapeBBox>) =
                match xs with
                | []    -> Point(maxX, maxY, maxZ), Point(minX, minY, minZ)
                | x::xs -> 
                    let maxX = if x.box.highPoint.X > maxX then x.box.highPoint.X else maxX
                    let maxY = if x.box.highPoint.Y > maxY then x.box.highPoint.Y else maxY
                    let maxZ = if x.box.highPoint.Z > maxZ then x.box.highPoint.Z else maxZ
                    let minX = if x.box.lowPoint.X  < minX then x.box.lowPoint.X  else minX
                    let minY = if x.box.lowPoint.Y  < minY then x.box.lowPoint.Y  else minY
                    let minZ = if x.box.lowPoint.Z  < minZ then x.box.lowPoint.Z  else minZ
                    find maxX minX maxY minY maxZ minZ xs
            find 0. 0. 0. 0. 0. 0. xs

    let partitionAfterSelect (boxes:list<ShapeBBox>) (splitX:float) (splitY:float) (splitZ:float) =
        let rec inner leftX rightX leftY rightY leftZ rightZ (tosort:list<ShapeBBox>) = 
            match tosort with
            | [] -> leftX,rightX,leftY,rightY,leftZ,rightZ
            | c::cr -> 

                let lX = if c.box.lowPoint.X < splitX then c::leftX
                         else leftX
                let rX = if c.box.highPoint.X > splitX then c::rightX
                         else rightX
                let lY = if c.box.lowPoint.Y < splitY then c::leftY
                         else leftY
                let rY = if c.box.highPoint.Y > splitY then c::rightY
                         else rightY
                let lZ = if c.box.lowPoint.Z < splitZ then c::leftZ
                         else leftZ
                let rZ = if c.box.highPoint.Z > splitZ then c::rightZ
                         else rightZ
                inner lX rX lY rY lZ rZ cr
        inner [] [] [] [] [] [] boxes

    let findSplitValues (boxes:list<ShapeBBox>) = 
        let boxesLength = float boxes.Length
        if boxesLength = 0. then 0., 0., 0.
        else
        let rec inner accX accY accZ (boxes:list<ShapeBBox>) = 
            match boxes with
            | [] -> (accX/boxesLength), (accY/boxesLength), (accZ/boxesLength)
            | x::xs -> 
                inner (accX+((x.box.highPoint.X+x.box.lowPoint.X)/2.))
                      (accY+((x.box.highPoint.Y+x.box.lowPoint.Y)/2.))
                      (accZ+((x.box.highPoint.Z+x.box.lowPoint.Z)/2.)) xs
        inner 0. 0. 0. boxes

    let mutable avgLeafSize = 0.

    let mutable totalLeafs = 0

    let mutable totalLeafSize = 0

    let mutable maxLeafSize = 0

    let rec createKDTreeFromList createRoot (boxes:list<ShapeBBox>) = 
        match boxes with
        | []    -> failwith "There are no shapes to build a tree with!"
        | boxes -> 
            let (KDMaxXYZ, KDMinXYZ) = if createRoot = true then findMaxMin boxes
                                       else Point(0., 0., 0.), Point(0., 0., 0.)
            
            if boxes.Length <= 1 then if boxes.Length > maxLeafSize then maxLeafSize <- boxes.Length
                                      totalLeafs <- totalLeafs+1
                                      totalLeafSize <- totalLeafSize+boxes.Length
                                      Leaf(BBox(KDMinXYZ, KDMaxXYZ), boxes)
            else
            let boxesLength = boxes.Length
            let (splitX, splitY, splitZ) = findSplitValues boxes

            let (firstX, secondX, firstY, secondY, firstZ, secondZ) = partitionAfterSelect boxes splitX splitY splitZ

            let (xIntersect, yIntersect, zIntersect) = ((((float firstX.Length)+(float secondX.Length))/(float boxes.Length)),
                                                        (((float firstY.Length)+(float secondY.Length))/(float boxes.Length)),
                                                        (((float firstZ.Length)+(float secondZ.Length))/(float boxes.Length)))

            let (first, second, splitValue, axis) = if xIntersect <= yIntersect && xIntersect <= zIntersect then
                                                        (firstX, secondX, splitX, 0)
                                                    else if yIntersect < xIntersect && yIntersect <= zIntersect then
                                                        (firstY, secondY, splitY, 1)
                                                    else (firstZ, secondZ, splitZ, 2)
            let firstLength = first.Length
            let secondLength = second.Length

            //printfn "%A" first
            //printfn "%A" second
            //printfn "%A" splitV
            //printfn "Original Length%A" boxes.Length
            //printfn "axis: %i" axis
            //printfn "%A %A" first.Length second.Length
            //printfn "partition"
            //printfn "splitvalue"
            //printfn "list filter"
            if  float((firstLength+secondLength))/(float(boxesLength)) > 1.3  then 
                if boxesLength > maxLeafSize then maxLeafSize <- boxesLength
                totalLeafs <- totalLeafs+1
                totalLeafSize <- totalLeafSize+boxesLength
                Leaf(BBox(KDMinXYZ, KDMaxXYZ),boxes)
            else if firstLength = boxesLength && secondLength = boxesLength then 
                if boxesLength > maxLeafSize then maxLeafSize <- boxesLength
                totalLeafs <- totalLeafs+1
                totalLeafSize <- totalLeafSize+boxesLength
                Leaf(BBox(KDMinXYZ, KDMaxXYZ),boxes)
            else if firstLength = boxesLength then 
                if boxesLength > maxLeafSize then maxLeafSize <- firstLength
                totalLeafs <- totalLeafs+1
                totalLeafSize <- totalLeafSize+firstLength
                Node(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), Leaf(BBox(KDMinXYZ, KDMaxXYZ),first), createKDTreeFromList false (second))
            else if secondLength = boxesLength then 
                if boxesLength > maxLeafSize then maxLeafSize <- secondLength
                totalLeafs <- totalLeafs+1
                totalLeafSize <- totalLeafSize+secondLength
                Node(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), createKDTreeFromList false (first), Leaf(BBox(KDMinXYZ, KDMaxXYZ),second))
            else Node(axis, splitValue, BBox(KDMinXYZ, KDMaxXYZ), createKDTreeFromList false (first), createKDTreeFromList false (second))
                
    let buildKDTree (shapes:array<Shape>) = 
        maxLeafSize <- 0
        totalLeafs <- 0
        totalLeafSize <- 0
        totalLeafs <- 0
        timer.Reset()
        let shapeBoxArray = Array.zeroCreate(shapes.Length)
        for i in 0..(shapes.Length-1) do
            let id = i
            let shape = shapes.[i]
            let newShapeBox = ShapeBBox(shape.getBoundingBox (), id)
            shapeBoxArray.[i] <- newShapeBox
        let ShapeBoxList = shapeBoxArray |> Array.toList
        let shapeString = if ShapeBoxList.Length = 1 then "shape" else "shapes"
        printfn "KD-build initialized with %A %s" ShapeBoxList.Length shapeString
        timer.Start()
        let kdTree = if shapeBoxArray.Length < 11 then 
                         let (KDMaxXYZ, KDMinXYZ) = findMaxMin ShapeBoxList
                         let leaf = Leaf(BBox(KDMinXYZ, KDMaxXYZ), ShapeBoxList) //Check for 10 shaper or less. If that is the case, no KD-tree will be built
                         timer.Stop()
                         printfn "KD-Leaf build in %f Seconds - 10 or less shapes were given" timer.Elapsed.TotalSeconds
                         leaf
                     else
                         let kdTree = createKDTreeFromList true ShapeBoxList
                         timer.Stop()
                         printfn "KD-Tree build in %f seconds" timer.Elapsed.TotalSeconds
                         printfn "Maximum shapes referenced in one Leaf: %A" maxLeafSize
                         printfn "Total leafs: %A" totalLeafs
                         printfn "Total references to shapes in leafs: %A" totalLeafSize
                         printfn "Avg leaf Size: %A" (totalLeafSize/totalLeafs)
                         kdTree
        printfn ""
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

    let closestHit (shapeBoxes:list<ShapeBBox>) (ray:Ray) (shapes:array<Shape>) =
        // Get closest hitpoint
        let rec findClosestHit (h:HitPoint) t' (shapeBoxes:list<ShapeBBox>) (shapes:array<Shape>) = 
            match shapeBoxes with
            | []    -> h
            | (s:ShapeBBox)::sl -> 
                       let hit = shapes.[s.shape].hitFunction ray
                       if hit.DidHit && hit.Time < t' then findClosestHit hit hit.Time sl shapes
                       else findClosestHit h t' sl shapes
        let hit = findClosestHit (HitPoint(ray)) infinity shapeBoxes shapes
        // Check if the ray hit
        if hit.DidHit then
            // If the ray hit, then return the first hit point
            Some (hit)
        else
            // If not, return none
            None
    
    let order (d:float, left:KDTree, right:KDTree) =
        if d > 0. then (left, right)
        else (right, left)


    let rec searchKDTree (tree:KDTree) (ray:Ray) (t:float) (t':float) (shapes:array<Shape>):HitPoint = 
        let SearchKDLeaf (tree:KDTree) (ray:Ray) (t':float) (shapes:array<Shape>) = 
            match tree with
            | Node(_) -> failwith "Should never be a node here..."
            | Leaf(_, shapeList) -> 
                let option = closestHit shapeList ray shapes
                match option with
                | Some(hit) -> if hit.Time < t' then hit
                               else HitPoint(ray)
                | None ->      HitPoint(ray)
        
        let searchKDNode (tree:KDTree) (ray:Ray) (t:float) (t':float) (shapes:array<Shape>) = 
            match tree with
            | Leaf(_) -> failwith "Should never be a leaf here..."
            | Node(axis, value, _, left, right) ->
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
        | Node(_) -> searchKDNode tree ray t t' shapes
        | Leaf(_) -> SearchKDLeaf tree ray t' shapes
                            

    let traverseKDTree (tree:KDTree) (ray:Ray) (shapes:array<Shape>) = 
        match tree with
        | Node(_, _, bBox, _, _) -> let intersect = bBox.intersect ray
                                    match intersect with
                                    | Some (t, t') -> searchKDTree tree ray t t' shapes
                                    | None -> HitPoint (ray)
        | Leaf(bBox, _)          -> let intersect = bBox.intersect ray
                                    match intersect with
                                    | Some (t, t') -> searchKDTree tree ray t t' shapes
                                    | None -> HitPoint (ray)