module TransformationTest

open Transformation
open Assert
let allTest = 
    let a = new assertion()
    printfn("Transformation Test")
    let testGetRowLengthWith3x5Returns3 = 
        let testMatrix = Transformation.mkTransformation [[0.;1.;0.;2.;1.];[10.;5.;4.;2.;6.];[13.;-4.;6.;1.;2.]]
        let result = Transformation.getRowLength testMatrix
        a.equal (3,result)

    let testGetColLengthWith3x5Returns5= 
        let testMatrix = Transformation.mkTransformation [[0.;1.;0.;2.;1.];[10.;5.;4.;2.;6.];[13.;-4.;6.;1.;2.]]
        let result = Transformation.getColLength testMatrix
        a.equal (5,result)

    let testMatrixMultiplicationEqualsCorrectMatrix = 
        let firstMatrix = Transformation.mkTransformation [[1.;2.];[3.;4.]]
        let secondMatrix = Transformation.mkTransformation [[4.;3.];[2.;1.]]
        let result = Transformation.getList (Transformation.multi (firstMatrix,secondMatrix))
        let expected = [[8.;5.];[20.;13.]]
        a.equal (expected, result)
    
    let testTranspose = 
        let matrix = Transformation.mkTransformation [[1.;2.];[3.;4.]]
        let result = Transformation.getList (Transformation.transpose matrix)
        let expected = [[1.;3.];[2.;4.]]
        a.equal (expected,result)

    testGetRowLengthWith3x5Returns3
    testGetColLengthWith3x5Returns5
    testMatrixMultiplicationEqualsCorrectMatrix
    testTranspose