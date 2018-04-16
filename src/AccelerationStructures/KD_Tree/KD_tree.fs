namespace Acceleration

module KD_tree = 

    open System.Linq

    type coordinate = { x:float; y:float; z:float }

    type Ray = R of float * float * float

    exception KDException

    type Shape = S of float

    (*type BBox1 = { maxY_lowX_maxZ:coordinate; maxY_maxX_maxZ:coordinate; 
              lowY_lowX_maxZ:coordinate; lowY_maxX_maxZ:coordinate; 
              maxY_lowX_lowZ:coordinate; maxY_maxX_lowZ:coordinate; 
              lowY_lowX_lowZ:coordinate; lowY_maxX_lowZ:coordinate; 
              shape:Shape }*)

    type BBox = { maxXYZ:coordinate; 
              minXYZ:coordinate;
              shape:Shape }

    type KDTree = Leaf of BBox list
                | Node of string * float * KDTree * KDTree

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


(*let rec qsortY (xs:list<BBox>) =
  match xs with
  | [] -> []
  | x :: xs ->
      let smaller = qsortY (xs |> List.filter(fun e -> e.maxXYZ.y <= x.maxXYZ.y))
      let larger  = qsortY (xs |> List.filter(fun e -> e.maxXYZ.y >  x.maxXYZ.y))
      smaller @ [x] @ larger
let rec qsortZ (xs:list<BBox>) =
  match xs with
  | [] -> []
  | x :: xs ->
      let smaller = qsortY (xs |> List.filter(fun e -> e.maxXYZ.z <= x.maxXYZ.z))
      let larger  = qsortY (xs |> List.filter(fun e -> e.maxXYZ.z >  x.maxXYZ.z))
      smaller @ [x] @ larger*)

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

(*let findMaxMinY (xs:list<BBox>) = 
    match xs with
    | []    -> (infinity, infinity)
    | x::xs -> let rec find (xs:list<BBox>) (max:float) (min:float) =
                   match xs with
                   | []    -> (max, min)
                   | x::xs -> if x.maxXYZ.y > max && x.minXYZ.y < min then find xs x.maxXYZ.y x.minXYZ.y
                              else if x.maxXYZ.y > max then find xs x.maxXYZ.y min
                              else if x.minXYZ.y < min then find xs max x.minXYZ.y
                              else find xs max min
               find xs x.maxXYZ.y x.minXYZ.y
            
let findMaxMinZ (xs:list<BBox>) = 
    match xs with
    | []    -> (infinity, infinity)
    | x::xs -> let rec find (xs:list<BBox>) (max:float) (min:float) =
                   match xs with
                   | []    -> (max, min)
                   | x::xs -> if x.maxXYZ.z > max && x.minXYZ.z < min then find xs x.maxXYZ.z x.minXYZ.z
                              else if x.maxXYZ.z > max then find xs x.maxXYZ.z min
                              else if x.minXYZ.z < min then find xs max x.minXYZ.z
                              else find xs max min
               find xs x.maxXYZ.z x.minXYZ.z*)


    let rec buildKDTree (boxes:list<BBox>) = 
        if (List.length boxes) < 10 then Leaf(boxes)
        else
        match boxes with
            | []    -> raise KDException
            | boxes -> let newBoxesX = boxes
                       let (MaxX, MinX) = findMaxMin newBoxesX 0
                       let newBoxesY = boxes 
                       let (MaxY, MinY) = findMaxMin newBoxesY 1
                       let newBoxesZ = boxes
                       let (MaxZ, MinZ) = findMaxMin newBoxesZ 2
                       let XDistance = MaxX - MinX
                       let YDistance = MaxY - MinY
                       let ZDistance = MaxZ - MinZ
                       let rec buildNode boxes visitX visitY visitZ tries = 
                            let buildNodeX boxes = 
                                    if List.length boxes = 1 then Leaf(boxes)
                                    else
                                    let oldBoxes = boxes
                                    let XsortedBoxes = qsort boxes 0
                                    let length = List.length XsortedBoxes
                                    let (first, second) = List.splitAt ((length/2)) XsortedBoxes
                                    let splitValue = first.[(List.length first)-1].maxXYZ.x
                                    let newSecond = second
                                    let firstlength = List.length first
                                    let secondLength = List.length second
                                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.x < splitValue) second)
                                    if ((List.length newFirst)-firstlength) > (((secondLength/100)+1)*60) then buildNode oldBoxes true false false (tries+1)
                                    else Node("x", splitValue, buildKDTree(newFirst), buildKDTree(newSecond))
                            let buildNodeY boxes = 
                                    if List.length boxes = 1 then Leaf(boxes)
                                    else
                                    let oldBoxes = boxes
                                    let YsortedBoxes = qsort boxes 1
                                    let length = List.length YsortedBoxes
                                    let (first, second) = List.splitAt ((length/2)) YsortedBoxes
                                    let splitValue = first.[(List.length first)-1].maxXYZ.y
                                    let newSecond = second
                                    let firstlength = List.length first
                                    let secondLength = List.length second
                                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.y < splitValue) second)
                                    if ((List.length newFirst)-firstlength) > (((secondLength/100)+1)*60) then buildNode oldBoxes false true false (tries+1)
                                    else Node("y", splitValue, buildKDTree(newFirst), buildKDTree(newSecond))
                            let buildNodeZ boxes = 
                                    if List.length boxes = 1 then Leaf(boxes)
                                    else
                                    let oldBoxes = boxes
                                    let ZsortedBoxes = qsort boxes 2
                                    let length = List.length ZsortedBoxes
                                    let (first, second) = List.splitAt ((length/2)) ZsortedBoxes
                                    let splitValue = first.[(List.length first)-1].maxXYZ.z
                                    let newSecond = second
                                    let firstlength = List.length first
                                    let secondLength = List.length second
                                    let newFirst = first @ (List.filter(fun n -> n.minXYZ.z < splitValue) second)
                                    if ((List.length newFirst)-firstlength) > (((secondLength/100)+1)*60) then buildNode oldBoxes false false true (tries+1)
                                    else Node("z", splitValue, buildKDTree(newFirst), buildKDTree(newSecond))
                            if visitX = false && tries < 3 then buildNodeX boxes
                            else if visitY = false && tries < 3 then buildNodeY boxes
                            else if visitZ = false && tries < 3 then buildNodeZ boxes
                            else Leaf(boxes)
                       if XDistance >= YDistance && XDistance >= ZDistance then buildNode boxes false true true 0
                       else if YDistance > XDistance && YDistance >= ZDistance then buildNode boxes true false true 0
                       else buildNode boxes true true false 0


    let rec traverseKDTree (tree:KDTree) (ray:Ray):BBox = failwith "Not Implemented"

    let rec searchKDTree node = failwith "Not Implemented"

    let rec searchKDLeaf node = failwith "Not Implemented"

    let rec searchKDNode node = failwith "Not Implemented"

    let rec KDHit leaf = failwith "Not Implemented"