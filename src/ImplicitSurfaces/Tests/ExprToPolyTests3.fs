namespace Tests

(* Testing that various implicit surface shapes can be parsed, and made into polynomials 
    Also some tests for
*)

module ExprToPolyTests3 =
  open Assert
  open Tracer.ImplicitSurfaces.ExprParse
  open Tracer.ImplicitSurfaces.ExprToPoly
  open Tracer.ImplicitSurfaces.Main

  let allTests =

    printfn "Doing tests on exprtopoly for simple shape equations"

    let test01 = 
      let input = (parseStr >> substWithRayVars) "x^2 + y^2 + z^2 - r^2"
      let inputradical = (parseStr >> substWithRayVars) "(x^2 + y^2 + z^2)_2 - r"
      let actual = ppPoly "t" (exprToPoly input "t")
      let actualradical = ppPoly "t" (exprToPoly inputradical "t")
      let expected = "(-1*r^2+oz^2+oy^2+ox^2)+t(2*dz*oz+2*dy*oy+2*dx*ox)+t^2(dz^2+dy^2+dx^2)"
      Assert.Equal actual expected "sphereTest1: sphere"
      Assert.Equal actualradical expected "sphereTest2: sphere with radical"
   


   (*
    
      let test02 =
        let input = (parseStr >> substWithRayVars) "(((x^2 + y^2)_2 - 2.0)^2 + z^2)_2 - 1.0"
        let actual = ppPoly "t" (exprToPoly input "t")
        let expected = 
      
      ppPoly "" (exprToPoly (parseStr "x_2 * y_2 + y^2 + 9") "")
      ppPoly "" (exprToPoly (parseStr "x_2 * y_3") "")
    
    *)

    test01