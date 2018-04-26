namespace Tests

module ImplicitSurfacesTests =

  open Tests.Assert
  
  let allTests =
  0

  (* Test string

  test "x^2 + y^2 + z^2 - 1"
  "(pz^2+py^2+px^2+-1)+t(2*dz*pz+2*dy*py+2*dx*px)+t^2(dz^2+dy^2+dx^2)"

  (parseStr >> substWithRay >> exprToPoly) "x^2 + y^2 + z^2 - 1" "t"

  Test strings from the TracerTestSuite > ImplicitSurfaces.fs

  sphere1:
  makeImplicit "x^2 + y^2 + z^2 - 9.9"
  
  sphere2:
  test "(x^2 + y^2 + z^2)_2 - 3.3"

  torus:
  test "(((x^2 + y^2)_2 - 1.1)^2 + z^2)_2 - 2.2"

  torus2:
  test "x^4 + 2x^2*y^2 + 2x^2*z^2 - 2*(2.2^2 + 1.1^2)*x^2 + y^4 + 2y^2*z^2 + 2*(2.2^2 - 1.1^2)*y^2 + z^4 - 2*(2.2^2 + 1.1^2)*z^2 + (2.2^2 - 1.1^2)^2"

  testShape:
  test "(x - 2)^2(x+2)^2 + (y - 2)^2(y+2)^2 + (z - 2)^2(z+2)^2 + 3(x^2*y^2 + x^2z^2 + y^2z^2) + 6x y z - 10(x^2 + y^2 + z^2) + 22"

  heart:
  test "(x^2 + (4.0/9.0)*y^2 + z^2 - 1)^3 - x^2 * z^3 - (9.0/80.0)*y^2*z^3"
  *)