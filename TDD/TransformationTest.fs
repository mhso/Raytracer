module TransformationTest

open Transformation
open System
open Assert
open Tracer.Basics

let allTest = 
    let testGetRowLengthWith3x5Returns3 = 
        let testMatrix = mkTransformation [[0.;1.;0.;2.;1.];[10.;5.;4.;2.;6.];[13.;-4.;6.;1.;2.]]
        let result = getRowLength testMatrix
        Assert.Equal (3,result,"getRowLengthWith3x5")

    let testGetColLengthWith3x5Returns5= 
        let testMatrix = mkTransformation [[0.;1.;0.;2.;1.];[10.;5.;4.;2.;6.];[13.;-4.;6.;1.;2.]]
        let result = getColLength testMatrix
        Assert.Equal (5,result, "GetColLengthWith3x5")

    let testMatrixMultiplicationEqualsCorrectMatrix = 
        let firstMatrix = mkTransformation [[1.;2.];[3.;4.]]
        let secondMatrix = mkTransformation [[4.;3.];[2.;1.]]
        let result = getList (Transformation.multi (firstMatrix,secondMatrix))
        let expected = [[8.;5.];[20.;13.]]
        Assert.Equal (expected, result, "MatrixMultiplicationEqualsCorrectMatrix")
    
    let testTranspose = 
        let matrix = mkTransformation [[1.;2.];[3.;4.]]
        let result = getList (transpose matrix)
        let expected = [[1.;3.];[2.;4.]]
        Assert.Equal (expected,result, "Transpose")
    
    let testGetListReturnsListAttachedToMatrix = 
        let matrix = getList (mkTransformation([[1.;2.];[3.;4.]]))
        let expected = [[1.;2.];[3.;4.]]
        Assert.Equal(expected, matrix, "GetListReturnsCorrectList")

    let testScale = 
        let result = getList (scale 2. 0.5 1.)
        let expected = [[2.;0.;0.;0.];[0.;0.5;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal (expected,result, "Scale")

    let testScaleInv = 
        let result = getList (scaleInv 2. 0.5 1.)
        let expected = [[-2.;0.;0.;0.];[0.;-0.5;0.;0.];[0.;0.;-1.;0.];[0.;0.;0.;1.]]
        Assert.Equal (expected,result, "ScaleInv")
    
    let testTranslate = 
        let matrix = getList (translate 10. 10. -10.)
        let expected = [[1.;0.;0.;10.];[0.;1.;0.;10.];[0.;0.;1.;-10.];[0.;0.;0.;1.]]
        Assert.Equal(expected, matrix, "TranslateTest")

    let testTranslateInv = 
        let matrix = getList (translateInv 10. 10. -10.)
        let expected = [[1.;0.;0.;-10.];[0.;1.;0.;-10.];[0.;0.;1.;10.];[0.;0.;0.;1.]]
        Assert.Equal(expected, matrix, "TranslateInverseTest")
    
    let testSheareXY = 
        let result = getList (sheareXY 10.)
        let expected = [[1.;0.;0.;0.];[10.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal(expected, result, "SheareXY")
    let testSheareXZ = 
        let result = getList (sheareXZ -7.)
        let expected = [[1.;0.;0.;0.];[0.;1.;0.;0.];[-7.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal(expected, result, "SheareXZ")
    let testSheareYX = 
        let result = getList (sheareYX 10.)
        let expected = [[1.;10.;0.;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal(expected, result, "SheareYX")
    let testSheareYZ = 
        let result = getList (sheareYZ 10.)
        let expected = [[1.;0.;0.;0.];[0.;1.;0.;0.];[0.;10.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal(expected, result, "SheareYZ")
    let testSheareZX = 
        let result = getList (sheareZX 10.)
        let expected = [[1.;0.;10.;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal(expected, result, "SheareZX")
    let testSheareZY = 
        let result = getList (sheareZY 10.)
        let expected = [[1.;0.;0.;0.];[0.;1.;10.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal(expected, result, "SheareZY")

    let testRotateXMultiplyInverseRotateXGivesIdentityMatrix = 
        let matrix1 = rotateX 1.5
        let matrix2 = rotateXInv 1.5
        let result = getList (Transformation.multi (matrix1,matrix2))
        let expected = [[1.0;0.0;0.;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal(expected,result,"RotateXMultiplyInverseRotateXGivesIdentityMatrix")

    let testRotateYMultiplyInverseRotateYGivesIdentityMatrix = 
        let matrix1 = rotateY 1.5
        let matrix2 = rotateYInv 1.5
        let result = getList (Transformation.multi (matrix1,matrix2))
        let expected = [[1.0;0.0;0.;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal(expected,result,"RotateYMultiplyInverseRotateYGivesIdentityMatrix")

    let testRotateZMultiplyInverseRotateZGivesIdentityMatrix = 
        let matrix1 = rotateZ 1.5
        let matrix2 = rotateZInv 1.5
        let result = getList (Transformation.multi (matrix1,matrix2))
        let expected = [[1.0;0.0;0.;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]
        Assert.Equal(expected,result,"RotateZMultiplyInverseRotateZGivesIdentityMatrix")

    let testMergeTransformationWithTwoTransformations = 
        let matrix1 = (scale 10. 10. 10.)
        let matrix2 = translate 2. 2. 2.
        let listMatrix = [matrix1; matrix2]
        let result = mergeTransformations listMatrix
        let expected = Transformation.multi (matrix1,matrix2)
        Assert.Equal(expected,result,"MergeTransformationWithTwoTransformations")

        
    let testMergeTransformationWithFiveTransformations = 
        let matrix1 = (scale 10. 10. 10.)
        let matrix2 = translate 2. 2. 2.
        let matrix3 = rotateX 10.
        let matrix4 = sheareXY 30.
        let matrix5 = scale -1. 1. 1.
        let listMatrix = [matrix1; matrix2; matrix3; matrix4; matrix5]
        let result = mergeTransformations listMatrix
        let expected = Transformation.multi ((Transformation.multi ((Transformation.multi ((Transformation.multi (matrix1,matrix2)),matrix3)),matrix4),matrix5))
        Assert.Equal(expected,result,"MergeTransformationWithFiveTransformations")

    let testTransformDirectionalLight = 
        let light = new DirectionalLight(new Colour(1.,1.,1.), 1., new Vector(0.,10.,0.))
        let move = rotateZ (Math.PI / 2.0)
        let result = ((transformLight light move) :?> DirectionalLight).Direction.GetCoord
        let expected = (new DirectionalLight(new Colour(1.,1.,1.), 1., new Vector(10.,0.,0.))).Direction.GetCoord
        Assert.Equal(expected,result, "TrasformingDirectionalLight")

    let testTransformPointLight = 
        let light = new PointLight(new Colour(1.,1.,1.), 1., new Point(0.,10.,0.))
        let move = translate 5. -20. 0.
        let result = ((transformLight light move) :?> PointLight).Position.GetCoord
        let expected = (new PointLight(new Colour(1.,1.,1.), 1., new Point(5.,-10.,0.))).Position.GetCoord
        Assert.Equal(expected,result, "TrasformingPointLight")

    0
    
