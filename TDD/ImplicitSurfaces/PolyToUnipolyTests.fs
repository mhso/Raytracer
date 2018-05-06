module PolyToUnipolyTests

open Assert
open Tracer.ImplicitSurfaces.PolyToUnipoly
open Tracer.ImplicitSurfaces.ExprToPoly
open Tracer.ImplicitSurfaces.ExprParse
open Tracer.ImplicitSurfaces.Main

let allTest =

  let test01 =
      let p = polyToUnipoly ((parseStr >> exprToPoly) "3x^3 + 3x^2 + 5x + 1" "x") Map.empty
      let actual = unipolyDerivative p
      let expected = UP (Map.empty
                          .Add(2, 9.0)
                          .Add(1, 6.0)
                          .Add(0, 5.0))
      Assert.Equal (expected, actual, "reducedPolyDerivative: 3x^3 + 3x^2 + 5x + 1 ; p' = 9x^2 + 6x + 5")

  let test02 =
    let p = polyToUnipoly ((parseStr >> exprToPoly) " - 3x^3 - 3x^2 - 5x - 1" "x") Map.empty
    let actual = unipolyDerivative p
    let expected = UP (Map.empty
                        .Add(2, -9.0)
                        .Add(1, -6.0)
                        .Add(0, -5.0))
    Assert.Equal (expected, actual, "reducedPolyDerivative: -3x^3 - 3x^2 - 5x - 1 ; p' = -9x^2 - 6x - 5")

  let test03 =
    let p = polyToUnipoly ((parseStr >> exprToPoly) "x^4 + 2x^2 + 4x + 1" "x") Map.empty
    let actual = sturmSeq p
    let expected = [UP (Map.empty 
                          .Add(0, 16.0)
                          .Add(1, 36.0)); 
                    UP (Map.empty
                          .Add(0, 1.0)
                          .Add(1, 3.0)
                          .Add(2, 1.0));
                    UP (Map.empty
                          .Add(0, 4.0)
                          .Add(1, 4.0)
                          .Add(3, 4.0));
                    UP (Map.empty
                          .Add(0, 1.0)
                          .Add(1, 4.0)
                          .Add(2, 2.0)
                          .Add(4, 1.0))]
    Assert.Equal (expected, actual, "simple Sturm seq")

  (*let test04 =
    let ex = (parseStr "(((x^2 + y^2)_2 - 1.5)^2 + z^2)_2 - 0.5")
    let input = substWithRayVars ex
    let actual = ppPoly "t" (exprToPoly input "t")
    printfn "%A" actual
    0*)

  test01
  test02
  test03
  //test04