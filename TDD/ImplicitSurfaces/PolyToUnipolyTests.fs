module PolyToUnipolyTests

open Assert
open Tracer.ImplicitSurfaces.PolyToUnipoly
open Tracer.ImplicitSurfaces.ExprToPoly
open Tracer.ImplicitSurfaces.ExprParse

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

  test01
  test02