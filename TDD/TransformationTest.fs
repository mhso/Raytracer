module TransformationTest

open Transformation
open Assert
let allTest = 
    printfn("Transformation Test")
    let testGetRowLengthWith3x5Returns3 = 
        let testMatrix = Transformation.mkTransformation [[0.;1.;0.;2.;1.];[10.;5.;4.;2.;6.];[13.;-4.;6.;1.;2.]]
        let result = Transformation.getRowLength testMatrix
        Assert.Equal (3,result,"getRowLengthWith3x5")

    let testGetColLengthWith3x5Returns5= 
        let testMatrix = Transformation.mkTransformation [[0.;1.;0.;2.;1.];[10.;5.;4.;2.;6.];[13.;-4.;6.;1.;2.]]
        let result = Transformation.getColLength testMatrix
        Assert.Equal (5,result, "GetColLengthWith3x5")

    let testMatrixMultiplicationEqualsCorrectMatrix = 
        let firstMatrix = Transformation.mkTransformation [[1.;2.];[3.;4.]]
        let secondMatrix = Transformation.mkTransformation [[4.;3.];[2.;1.]]
        let result = Transformation.getList (Transformation.multi (firstMatrix,secondMatrix))
        let expected = [[8.;5.];[20.;13.]]
        Assert.Equal (expected, result, "MatrixMultiplicationEqualsCorrectMatrix")
    
    let testTranspose = 
        let matrix = Transformation.mkTransformation [[1.;2.];[3.;4.]]
        let result = Transformation.getList (Transformation.transpose matrix)
        let expected = [[1.;3.];[2.;4.]]
        Assert.Equal (expected,result, "Transpose")
    
    let testScale = 
        let result = Transformation.getList (Transformation.scale 2. 0.5 1.)
        let expected = [[2.;0.;0.;0.];[0.;0.5;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal (expected,result, "Scale")

    testGetRowLengthWith3x5Returns3
    testGetColLengthWith3x5Returns5
    testMatrixMultiplicationEqualsCorrectMatrix
    testTranspose