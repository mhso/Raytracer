module RegularGridsTest

open RegularGrids
open Assert
let allTest = 
    let clampTest_X_lessThan_0_Return_0 = 
        let expected = 0.0
        let result = clamp (-1.0, 5.0)
        Assert.Equal (expected, result, "clampTest_X_lessThan_0_Return_0")
    clampTest_X_lessThan_0_Return_0

    let clampTest_X_largerThan_B_Return_B =
        let expected = 5.0
        let result = clamp (10.0, 5.0)
        Assert.Equal (expected, result, "clampTest_X_largerThan_B_Return_B")
    clampTest_X_largerThan_B_Return_B

    let clampTest_OtherWise_Return_X =
        let expected = 4.0
        let result = clamp (4.0, 5.0)
        Assert.Equal (expected, result, "clampTest_OtherWise_Return_X")
    clampTest_OtherWise_Return_X
        