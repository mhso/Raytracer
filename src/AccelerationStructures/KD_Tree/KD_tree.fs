namespace Acceleration

module KD_tree = 

    open System.Linq
    open System.Diagnostics.Contracts

    type coordinate = { x:float; y:float; z:float }

    type Ray = R of float * float * float

    exception KDException

    type Shape = S of float

    type ShapeBBox = { maxXYZ:coordinate; 
                       minXYZ:coordinate;
                       shape:int }

    type BBox = { maxXYZ:coordinate; 
                  minXYZ:coordinate }

    type KDTree = Leaf of BBox * ShapeBBox list
                | Node of string * float * BBox * KDTree * KDTree

    let rec qsort (xs:list<ShapeBBox>) axis =
        match xs with
        | [] -> []
        | x :: xs -> 
            let small, large = 
                match axis with 
                | 0 -> let filterSmall = fun (e:ShapeBBox) -> e.maxXYZ.x <= x.maxXYZ.x
                       let filterLarger = fun (e:ShapeBBox) -> e.maxXYZ.x >  x.maxXYZ.x
                       filterSmall, filterLarger
                | 1 -> let filterSmall = fun (e:ShapeBBox) -> e.maxXYZ.y <= x.maxXYZ.y
                       let filterLarger = fun (e:ShapeBBox) -> e.maxXYZ.y >  x.maxXYZ.y
                       filterSmall, filterLarger
                | _ -> let filterSmall = fun (e:ShapeBBox) -> e.maxXYZ.z <= x.maxXYZ.z
                       let filterLarger = fun (e:ShapeBBox) -> e.maxXYZ.z >  x.maxXYZ.z
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
                    | 0 -> if x.maxXYZ.x > max && x.minXYZ.x < min then find xs x.maxXYZ.x x.minXYZ.x axis
                           else if x.maxXYZ.x > max then find xs x.maxXYZ.x min axis
                           else if x.minXYZ.x < min then find xs max x.minXYZ.x axis
                           else find xs max min axis
                    | 1 -> if x.maxXYZ.y > max && x.minXYZ.y < min then find xs x.maxXYZ.y x.minXYZ.y axis
                           else if x.maxXYZ.y > max then find xs x.maxXYZ.y min axis
                           else if x.minXYZ.y < min then find xs max x.minXYZ.y axis
                           else find xs max min axis
                    | _ -> if x.maxXYZ.z > max && x.minXYZ.z < min then find xs x.maxXYZ.z x.minXYZ.z axis
                           else if x.maxXYZ.z > max then find xs x.maxXYZ.z min axis
                           else if x.minXYZ.z < min then find xs max x.minXYZ.z axis
                           else find xs max min axis
            find xs x.maxXYZ.x x.minXYZ.x axis


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
            let KDMaxXYZ = {x = MaxX; y = MaxY; z = MaxZ}
            let KDMinXYZ = {x = MinX; y = MinY; z = MinZ}
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
                    let splitValue = first.[(List.length first)-1].maxXYZ.x
                    let newSecond = second
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.x < splitValue) second)
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
                    let splitValue = first.[(List.length first)-1].maxXYZ.y
                    let newSecond = second
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.y < splitValue) second)
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
                    let splitValue = first.[(List.length first)-1].maxXYZ.z
                    let newSecond = second
                    let firstlength = float(List.length first)
                    let secondLength = float(List.length second)
                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.z < splitValue) second)
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

    let rec intersect bbox ray = failwith "Not Implemented"

    let rec searchKDTree node ray t t' = failwith "Not Implemented"

    let traverseKDTree (tree:KDTree) (ray:Ray) = 
        match tree with
        | Node(s, split, bbox, left, right) as n -> if intersect bbox ray = Some (t, t') then searchKDTree n ray t t'
                                                    else None
        | Leaf(bbox, boxes) as L                 -> if intersect bbox ray = Some (t, t') then searchKDTree L ray t t'
                                                    else None

    let rec searchKDLeaf node = failwith "Not Implemented"

    let rec searchKDNode node = failwith "Not Implemented"

    let rec KDHit leaf = failwith "Not Implemented"

    let BBox1 = {maxXYZ = {x = 4.0; y = 4.0; z = 4.0};
                 minXYZ = {x = 3.0; y = 3.0; z = 3.0};
                 shape = 1}
    let BBox2 = {maxXYZ = {x = 3.0; y = 3.0; z = 3.0};
                 minXYZ = {x = 2.0; y = 2.0; z = 2.0};
                 shape = 2}
    let BBox3 = {maxXYZ = {x = 2.0; y = 2.0; z = 2.0};
                 minXYZ = {x = -1.0; y = -1.0; z = -1.0};
                 shape = 3}
    let BBox4 = {maxXYZ = {x = 1.0; y = 1.0; z = 1.0};
                 minXYZ = {x = 0.0; y = 0.0; z = 0.0};
                 shape = 4}
    let BBox5 = {maxXYZ = {x = 0.0; y = 0.0; z = 0.0};
                 minXYZ = {x = -1.0; y = -1.0; z = -1.0};
                 shape = 5}
    let BBox6 = {maxXYZ = {x = -4.0; y = -5.0; z = -5.0};
                 minXYZ = {x = -7.0; y = -7.0; z = -7.0};
                 shape = 6}
    
    let BBList1 = [BBox1;BBox2;BBox3;BBox4;BBox5;BBox6]

    buildKDTree BBList1;;
