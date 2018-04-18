namespace Acceleration

module KD_tree = 

    open System.Linq
    open System.Diagnostics.Contracts

    type coordinate = { x:float; y:float; z:float }

    type Ray = R of float * float * float

    exception KDException

    type Shape = S of float

    type BBox = { maxXYZ:coordinate; 
                  minXYZ:coordinate;
                  shape:Shape }

    type KDTree = Leaf of BBox list
                | Node of string * float * float * KDTree * KDTree

    let rec qsort (xs:list<BBox>) axis =
        match xs with
            | [] -> []
            | x :: xs -> let small, large = 
                            match axis with 
                                | 0 -> let filterSmall = fun e -> e.maxXYZ.x <= x.maxXYZ.x
                                       let filterLarger = fun e -> e.maxXYZ.x >  x.maxXYZ.x
                                       filterSmall, filterLarger
                                | 1 -> let filterSmall = fun e -> e.maxXYZ.y <= x.maxXYZ.y
                                       let filterLarger = fun e -> e.maxXYZ.y >  x.maxXYZ.y
                                       filterSmall, filterLarger
                                | _ -> let filterSmall = fun e -> e.maxXYZ.z <= x.maxXYZ.z
                                       let filterLarger = fun e -> e.maxXYZ.z >  x.maxXYZ.z
                                       filterSmall, filterLarger
                         let smaller = qsort (xs |> List.filter(small)) axis
                         let larger  = qsort (xs |> List.filter(large)) axis
                         smaller @ [x] @ larger


    let findMaxMin (xs:list<BBox>) axis = 
        match xs with
            | []    -> (infinity, infinity)
            | x::xs -> let rec find (xs:list<BBox>) (max:float) (min:float) (axis:int) =
                            match xs with
                                | []    -> (max, min)
                                | x::xs -> match axis with 
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
        printfn "%A %A %A" xVisited yVisited zVisited
        match (xVisited, yVisited, zVisited) with
            | (false, false, false) -> if      xDistance >= yDistance && xDistance >= zDistance then (true, yVisited, zVisited, 0)
                                       else if yDistance > xDistance && yDistance >= zDistance then (xVisited, true, zVisited, 1)
                                       else 
                                       (xVisited, yVisited, true, 2)
            | (true, false, false)  -> if      yDistance >= zDistance then (xVisited, true, zVisited, 1)
                                       else 
                                       (xVisited, yVisited, true, 2)
            | (false, true, false)  -> if      xDistance >= zDistance then (true, yVisited, zVisited, 0)
                                       else 
                                       (xVisited, yVisited, true, 2)
            | (false, false, true)  -> if      xDistance > yDistance then (true, yVisited, zVisited, 0)
                                       else 
                                       (xVisited, true, zVisited, 1)
            | (true, true, false)   -> (xVisited, yVisited, true, 2)
            | (false, true, true)   -> (true, yVisited, zVisited, 0)
            | (true, false, true)   -> (xVisited, true, zVisited, 1)
            | (true, true, true)    -> (xVisited, yVisited, zVisited, 3)


    let rec buildKDTree (boxes:list<BBox>) = 
        printfn "KD Branch build start"
        //if (List.length boxes) < 10 then Leaf(boxes) //Check for less than 10 shapes. If that is the case, no KD-tree will be built
        //else
        match boxes with
            | []    -> raise KDException
            | boxes -> let newBoxesX = boxes        //All the 8 lines below is all set up for the longest-axis check
                       let (MaxX, MinX) = findMaxMin newBoxesX 0
                       let newBoxesY = boxes 
                       let (MaxY, MinY) = findMaxMin newBoxesY 1
                       let newBoxesZ = boxes
                       let (MaxZ, MinZ) = findMaxMin newBoxesZ 2
                       let XDistance = MaxX - MinX
                       let YDistance = MaxY - MinY
                       let ZDistance = MaxZ - MinZ
                       let rec buildNode boxes (xVisited, yVisited, zVisited, axis) = 
                            let buildNodeX boxes = 
                                    printfn "%A" xVisited
                                    printfn "%A" axis
                                    printfn "SplitX"
                                    if List.length boxes <= 1 then Leaf(boxes)
                                    else
                                    printfn "OldBox = new"
                                    let oldBoxes = boxes
                                    printfn "Qsort..."
                                    let XsortedBoxes = qsort boxes 0
                                    printfn "Set length"
                                    let length = List.length XsortedBoxes
                                    printfn "Split List"
                                    let (first, second) = List.splitAt ((length/2)) XsortedBoxes
                                    printfn "Get Split value"
                                    let splitValue = first.[(List.length first)-1].maxXYZ.x
                                    let splitLow = first.[(List.length first)-1].minXYZ.x
                                    printfn "Set new second list"
                                    let newSecond = second
                                    printfn "firstlength"
                                    let firstlength = List.length first
                                    printfn "secondlength"
                                    let secondLength = List.length second
                                    printfn "Set new first list"
                                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.x < splitValue) second)
                                    printfn "Check intersection and recurve"
                                    printfn "%A" newFirst
                                    printfn "%A" newSecond
                                    if ((List.length newFirst)-firstlength) > (((secondLength/100)+1)*60) then buildNode oldBoxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                                    else if List.length newFirst = List.length oldBoxes && List.length newSecond = List.length oldBoxes then Leaf(oldBoxes)
                                    else if List.length newFirst = List.length oldBoxes then Node("x", splitValue, splitLow, buildKDTree(newSecond), Leaf(newFirst))
                                    else if List.length newSecond = List.length oldBoxes then Node("x", splitValue, splitLow, buildKDTree(newFirst), Leaf(newSecond))
                                    else Node("x", splitValue, splitLow, buildKDTree(newSecond), buildKDTree(newFirst))
                            let buildNodeY boxes = 
                                    if List.length boxes = 1 then Leaf(boxes)
                                    else
                                    let oldBoxes = boxes
                                    let YsortedBoxes = qsort boxes 1
                                    let length = List.length YsortedBoxes
                                    let (first, second) = List.splitAt ((length/2)) YsortedBoxes
                                    let splitValue = first.[(List.length first)-1].maxXYZ.y
                                    let splitLow = first.[(List.length first)-1].minXYZ.y
                                    let newSecond = second
                                    let firstlength = List.length first
                                    let secondLength = List.length second
                                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.y < splitValue) second)
                                    if ((List.length newFirst)-firstlength) > (((secondLength/100)+1)*60) then buildNode oldBoxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                                    else if List.length newFirst = List.length oldBoxes && List.length newSecond = List.length oldBoxes then Leaf(oldBoxes)
                                    else if List.length newFirst = List.length oldBoxes then Node("y", splitValue, splitLow, buildKDTree(newSecond), Leaf(newFirst))
                                    else if List.length newSecond = List.length oldBoxes then Node("y", splitValue, splitLow, buildKDTree(newFirst), Leaf(newSecond))
                                    else Node("y", splitValue, splitLow, buildKDTree(newSecond), buildKDTree(newFirst))
                            let buildNodeZ boxes = 
                                    if List.length boxes = 1 then Leaf(boxes)
                                    else
                                    let oldBoxes = boxes
                                    let ZsortedBoxes = qsort boxes 2
                                    let length = List.length ZsortedBoxes
                                    let (first, second) = List.splitAt ((length/2)) ZsortedBoxes
                                    let splitValue = first.[(List.length first)-1].maxXYZ.z
                                    let splitLow = first.[(List.length first)-1].minXYZ.z
                                    let newSecond = second
                                    let firstlength = List.length first
                                    let secondLength = List.length second
                                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.z < splitValue) second)
                                    if ((List.length newFirst)-firstlength) > (((secondLength/100)+1)*60) then buildNode oldBoxes (findNextAxis (XDistance, YDistance, ZDistance, xVisited, yVisited, zVisited))
                                    else if List.length newFirst = List.length oldBoxes && List.length newSecond = List.length oldBoxes then Leaf(oldBoxes)
                                    else if List.length newFirst = List.length oldBoxes then Node("z", splitValue, splitLow, buildKDTree(newSecond), Leaf(newFirst))
                                    else if List.length newSecond = List.length oldBoxes then Node("z", splitValue, splitLow, buildKDTree(newFirst), Leaf(newSecond))
                                    else Node("z", splitValue, splitLow, buildKDTree(newSecond), buildKDTree(newFirst))
                            if axis = 0 then buildNodeX boxes
                            else if axis = 1 then buildNodeY boxes
                            else if axis = 2 then buildNodeZ boxes
                            else Leaf(boxes)
                       buildNode boxes (findNextAxis (XDistance, YDistance, ZDistance, false, false, false))

    

    let rec traverseKDTree (tree:KDTree) (ray:Ray):BBox = failwith "Not Implemented"

    let rec searchKDTree node = failwith "Not Implemented"

    let rec searchKDLeaf node = failwith "Not Implemented"

    let rec searchKDNode node = failwith "Not Implemented"

    let rec KDHit leaf = failwith "Not Implemented"

    let BBox1 = {maxXYZ = {x = 4.0; y = 4.0; z = 4.0};
                 minXYZ = {x = 3.0; y = 3.0; z = 3.0};
                 shape = S(1.0)}
    let BBox2 = {maxXYZ = {x = 3.0; y = 3.0; z = 3.0};
                 minXYZ = {x = 2.0; y = 2.0; z = 2.0};
                 shape = S(1.0)}
    let BBox3 = {maxXYZ = {x = 2.0; y = 2.0; z = 2.0};
                 minXYZ = {x = -1.0; y = -1.0; z = -1.0};
                 shape = S(1.0)}
    let BBox4 = {maxXYZ = {x = 1.0; y = 1.0; z = 1.0};
                 minXYZ = {x = 0.0; y = 0.0; z = 0.0};
                 shape = S(1.0)}
    let BBox5 = {maxXYZ = {x = 0.0; y = 0.0; z = 0.0};
                 minXYZ = {x = -1.0; y = -1.0; z = -1.0};
                 shape = S(1.0)}
    let BBox6 = {maxXYZ = {x = -4.0; y = -5.0; z = -5.0};
                 minXYZ = {x = -7.0; y = -7.0; z = -7.0};
                 shape = S(1.0)}
    
    let BBList1 = [BBox1;BBox2;BBox3;BBox4;BBox5;BBox6]

    buildKDTree BBList1;;
