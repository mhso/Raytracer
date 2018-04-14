open Matrix
open System
let testGetRowLengthWith3x5Returns3 = 
    let testMatrix = Matrix.mkMatrix [[0;1;0;2;1];[10;5;4;2;6];[13;-4;6;1;2]]

    let result = Matrix.getRowLength testMatrix
    match result with
    | 3 -> printfn "TEST PASSED!"
    | _ -> failwith "TEST FAILED"

let testGetColLengthWith3x5Returns5= 
    let testMatrix = Matrix.mkMatrix [[0;1;0;2;1];[10;5;4;2;6];[13;-4;6;1;2]]

    let result = Matrix.getColLength testMatrix
    match result with
    | 5 -> printfn "TEST PASSED!"
    | _ -> failwith "TEST FAILED"

let testMatrixMultiplicationEqualsCorrectMatrix = 
    let firstMatrix = Matrix.mkMatrix [[1;2];[3;4]]
    let secondMatrix = Matrix.mkMatrix [[4;3];[2;1]]

    let result = ( * ) firstMatrix secondMatrix
    let expected = Matrix.mkMatrix [[8;5];[20;13]]
    let expString = expected.ToString
    match result.ToString with
    | expString -> printfn "TEST PASSED!"
    | _ -> failwith "TEST FAILED"

//let listOfTrans = 
//    let rnd = new Random()
//    let rec listTest v list = 
//        match v with
//        | 10000 -> list
//        | _ -> 
//            let findMatrix = 
//                let nextFunction = rnd.Next(3)
//                match nextFunction with
//                | 0 -> 
//                    let x = rnd.Next(100)
//                    scale (float x) (float x) (float x)
//                | 1 ->
//                    let mirror = rnd.Next(3)
//                    match mirror with
//                    | 0 -> mirrorX
//                    | 1 -> mirrorY 
//                    | _ -> mirrorZ
//                | 2 -> 
//                    let sheare = rnd.Next(5)
//                    let d = (rnd.Next(300))
//                    let dist = (float d)
//                    match sheare with
//                    | 0 -> sheareXY dist
//                    | 1 -> sheareXZ dist
//                    | 2 -> sheareYX dist
//                    | 3 -> sheareYZ dist
//                    | 4 -> sheareZX dist
//                    | _ -> sheareZY dist

//                | _ -> 
//                    let rotate = rnd.Next(2)
//                    match rotate with
//                    | 0 -> rorateX (Math.PI/2.)
//                    | 1 -> rotateY (Math.PI/2.)
//                    | _ -> rotateZ (Math.PI/2.)
//            let matrix = findMatrix
//            let newList = matrix::list
//            listTest (v+1) newList
//    let tlist = [mkTransformation ([[1.];[1.];[1.];[1.]])]
//    listTest 0 tlist
type Matrix = Transformation.Transformation
    
