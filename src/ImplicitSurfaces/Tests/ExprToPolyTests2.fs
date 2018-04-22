namespace Tests

module ExprToPolyTests2 = 

  open Tracer.ImplicitSurfaces.ExprParse
  open Tracer.ImplicitSurfaces.ExprToPoly
  open Tests.Assert

  let allTests = 
    printfn "Doing rewrite rules tests...."
    (*
      All these tests are based on the rules listed on page 41 in the Lecture notes (updated 14 April) document
    *)

    // case 1:
    let test01 = 
      let actual = (rewriteExpr << parseStr) "e^0"
      let expected = [[ANum 1.0]]
      Assert.Equal actual expected "rewriteRule1: e^0 = 1"

    // case 2:
    let test02 =
      let actual = (rewriteExpr << parseStr) "e^1"
      let expected = [[AExponent ("e",1)]]
      Assert.Equal actual expected "rewriteRule2: e^1 = e"

    // case 3:
    let test03 =
      let actual = (rewriteExpr << parseStr) "e^3"
      let expected = [[AExponent ("e",1);AExponent ("e",1);AExponent ("e",1)]]
      Assert.Equal actual expected "rewriteRule3: e^3 = e*e*e"

    // case 4:
    let test04 = 
      let actual = (rewriteExpr << parseStr) "e1*(e2 + e3)"
      let expected = [[AExponent ("e2",1);AExponent ("e1",1)];[AExponent ("e3",1);AExponent ("e1",1)]]
      Assert.Equal actual expected "rewriteRule4: e1*(e2+e3) = e2 * e1 + e3 * e1"

    // case 4:
    let test05 =
      let actual = (rewriteExpr << parseStr) "(e2 + e3) * e1"
      let expected = [[AExponent ("e1",1);AExponent ("e2",1)];[AExponent ("e1",1);AExponent ("e3",1)]]
      Assert.Equal actual expected "rewriteRule4: Mult on right side"

    // case 5:
    let test06 =
      let actual = (rewriteExpr << parseStr) "e1 * (e2 - e3)"
      let expected = [[AExponent ("e2",1);AExponent ("e1",1)];[AExponent ("e3",1);ANum -1.0;AExponent ("e1",1)]]
      Assert.Equal actual expected "rewriteRule5: e1 * (e2 - e3) = e1 * e2 - e1 * e3"

    // case 5:
    let test07 =
      let actual = (rewriteExpr << parseStr) "(e2 - e3) * e1"
      let expected = [[AExponent ("e1",1);AExponent ("e2",1)];[AExponent ("e1",1);AExponent ("e3",1);ANum -1.0]]
      Assert.Equal actual expected "rewriteRule5: Mult on right side"

    // case 6:
    let test08 =
      let actual = (rewriteExpr << parseStr) "e1 * (e2 / (e3 + 1))"
      let expected = [[AExponent("e2",1);AExponent("e1",1)]]
      Assert.Equal actual expected "rewriteRule6: e1 * (e2 / (e3 + 1)) = e1 * e2"

    // case 7:
    let test09 =
      let actual = (rewriteExpr << parseStr) "(e1 / e2) / e3"
      let expected = [[AExponent("e1",1)]]
      Assert.Equal actual expected "rewriteRule7: (e1 / e2) / e3 = e1"

    // case 8:
    let test10 = 
      let actual = (rewriteExpr << parseStr)"e1 / (e2 / e3)"
      let expected = [[AExponent("e3",1);AExponent("e1",1)]]
      Assert.Equal actual expected "rewriteRule8: e1 / (e2 / e3) = e1 * e3)"

    // case 9:
    let test11 =
      let actual = (rewriteExpr << parseStr) "e1 + (e2/e3)"
      let expected = [[AExponent("e3",1);AExponent("e1",1)];[AExponent("e2",1)]]
      Assert.Equal actual expected "rewriteRule9: e1 + (e2 / e3) = (e1 * e3 + e2)"
  
    // case 9:
    let test12 =
      let actual = (rewriteExpr << parseStr) "(e2/e3) + e1"
      let expected = [[AExponent("e3",1);AExponent("e1",1)];[AExponent("e2",1)]]
      Assert.Equal actual expected "rewriteRule9: Add on right side"
    
    // case 10:
    let test13 = 
      let actual = (rewriteExpr << parseStr) "(e1 / e2) / (e3 / e4)"
      let expected = [[AExponent("e4",1);AExponent("e1",1)]]
      Assert.Equal actual expected "rewriteRule10: (e1 * e4) / (e2 * e3) = e1 * e4"

    // case 11:
    let test14 =
      let actual = (rewriteExpr << parseStr) "e_3 * e_3 * e_3"
      let expected = [[AExponent("e",1)]]
      Assert.Equal actual expected "rewriteRule11: (e_3 * e_3 * e_3) = e"

    test01
    test02
    test03
    test04
    test05
    test06
    test07
    test08
    test09
    test10
    test11
    test12
    test13
    test14