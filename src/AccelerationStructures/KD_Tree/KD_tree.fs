namespace Acceleration

open Tracer.Basics

module KD_tree = 

    open System.Linq
    open System.Diagnostics.Contracts

    type coordinate = { x:float; y:float; z:float }


    exception KDException

    type Shape = S of float

    type ShapeBBox = { maxXYZ:Point; 
                       minXYZ:Point;
                       shape:int }

    type BBox = { maxXYZ:Point; 
                  minXYZ:Point }

    type KDTree = Leaf of BBox * ShapeBBox list
                | Node of string * float * BBox * KDTree * KDTree

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
            find xs x.maxXYZ.X x.minXYZ.X axis


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


    let rec buildKDTree (boxes:list<ShapeBBox>) = 
        printfn "KD Branch build start"
        //if (List.length boxes) < 10 then Leaf(boxes) //Check for less than 10 shapes. If that is the case, no KD-tree will be built
        //else
        match boxes with
        | []    -> raise KDException
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
                let buildNodeX boxes = 
                    printfn "Split X"
                    if List.length boxes <= 1 then Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, boxes)
                    else
                    let oldBoxes = boxes
                    let XsortedBoxes = qsort boxes 0
                    let length = List.length XsortedBoxes
                    let (first, second) = List.splitAt ((length/2)) XsortedBoxes
                    let splitValue = first.[(List.length first)-1].maxXYZ.X
                    let newSecond = second
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.X < splitValue) second)
                    if ((float(List.length newFirst))-firstlength) > (((secondLength*60.))/100.) then buildNode oldBoxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                    else if List.length newFirst = List.length oldBoxes && List.length newSecond = List.length oldBoxes then Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},oldBoxes)
                    else if List.length newFirst = List.length oldBoxes then Node("x", splitValue, {maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, buildKDTree(newSecond), Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},newFirst))
                    else if List.length newSecond = List.length oldBoxes then Node("x", splitValue, {maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, buildKDTree(newFirst), Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},newSecond))
                    else Node("x", splitValue, {maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, buildKDTree(newSecond), buildKDTree(newFirst))
                let buildNodeY boxes = 
                    printfn "Split Y"
                    if List.length boxes = 1 then Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},boxes)
                    else
                    let oldBoxes = boxes
                    let YsortedBoxes = qsort boxes 1
                    let length = List.length YsortedBoxes
                    let (first, second) = List.splitAt ((length/2)) YsortedBoxes
                    let splitValue = first.[(List.length first)-1].maxXYZ.Y
                    let newSecond = second
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.Y < splitValue) second)
                    if ((float(List.length newFirst))-firstlength) > (((secondLength*60.))/100.) then buildNode oldBoxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                    else if List.length newFirst = List.length oldBoxes && List.length newSecond = List.length oldBoxes then Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},oldBoxes)
                    else if List.length newFirst = List.length oldBoxes then Node("y", splitValue, {maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, buildKDTree(newSecond), Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},newFirst))
                    else if List.length newSecond = List.length oldBoxes then Node("y", splitValue, {maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, buildKDTree(newFirst), Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},newSecond))
                    else Node("y", splitValue, {maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, buildKDTree(newSecond), buildKDTree(newFirst))
                let buildNodeZ boxes = 
                    printfn "Split Z"
                    if List.length boxes = 1 then Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},boxes)
                    else
                    let oldBoxes = boxes
                    let ZsortedBoxes = qsort boxes 2
                    let length = List.length ZsortedBoxes
                    let (first, second) = List.splitAt ((length/2)) ZsortedBoxes
                    let splitValue = first.[(List.length first)-1].maxXYZ.Z
                    let newSecond = second
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.Z < splitValue) second)
                    if ((float(List.length newFirst))-firstlength) > (((secondLength*60.))/100.) then buildNode oldBoxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                    else if List.length newFirst = List.length oldBoxes && List.length newSecond = List.length oldBoxes then Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},oldBoxes)
                    else if List.length newFirst = List.length oldBoxes then Node("z", splitValue, {maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, buildKDTree(newSecond), Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},newFirst))
                    else if List.length newSecond = List.length oldBoxes then Node("z", splitValue, {maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, buildKDTree(newFirst), Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},newSecond))
                    else Node("z", splitValue, {maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ}, buildKDTree(newSecond), buildKDTree(newFirst))
                if axis = 0 then buildNodeX boxes
                else if axis = 1 then buildNodeY boxes
                else if axis = 2 then buildNodeZ boxes
                else Leaf({maxXYZ = KDMaxXYZ; minXYZ = KDMinXYZ},boxes)
            buildNode boxes (findNextAxis (XDistance, YDistance, ZDistance, false, false, false))

    

    let rec searchKDTree node ray t t' = failwith "Not Implemented"

    let traverseKDTree (tree:KDTree) (ray:Ray) = 
        match tree with
        | Node(s, split, bbox, left, right) as n -> let value = intersect bbox ray 
                                                    match value with 
                                                    | Some (t, t') -> searchKDTree n ray t t'
                                                    | None -> None
        | Leaf(bbox, boxes) as L                 -> let value = intersect bbox ray 
                                                    match value with 
                                                    | Some (t, t') -> searchKDTree L ray t t'
                                                    | None -> None

    let rec searchKDLeaf node = failwith "Not Implemented"

    let rec searchKDNode node = failwith "Not Implemented"

    let rec KDHit leaf = failwith "Not Implemented"

    let BBox1 = {maxXYZ = Point(4.0, 4.0, 4.0);
                 minXYZ = Point(3.0, 3.0, 3.0);
                 shape = 1}
    let BBox2 = {maxXYZ = Point(3.0, 3.0, 3.0);
                 minXYZ = Point(2.0, 2.0, 2.0);
                 shape = 2}
    let BBox3 = {maxXYZ = Point(2.0, 2.0, 2.0);
                 minXYZ = Point(-1.0, -1.0, -1.0);
                 shape = 3}
    let BBox4 = {maxXYZ = Point(1.0, 1.0, 1.0);
                 minXYZ = Point(0.0, 0.0, 0.0);
                 shape = 4}
    let BBox5 = {maxXYZ = Point(0.0, 0.0, 0.0);
                 minXYZ = Point(-1.0, -1.0, -1.0);
                 shape = 5}
    let BBox6 = {maxXYZ = Point(-4.0, -5.0, -5.0);
                 minXYZ = Point(-7.0, -7.0, -7.0);
                 shape = 6}
    
    let BBList1 = [BBox1;BBox2;BBox3;BBox4;BBox5;BBox6]

    buildKDTree BBList1;;
