﻿namespace Tracer.ImplicitSurfaces

module Program =

  open System.Drawing
  open Tracer.Basics
  open Tracer.ImplicitSurfaces.Main
  open Tracer.Basics

  type baseShape = Main.baseShape
  type shape = Main.shape

  [<EntryPoint>]
  let main _ = 

    // the scene
    let mkScene' s (c:Camera) =
      let light = PointLight (Colour.White, 0.5, Point(4.0, 2.0, 4.0))
      let light2 = PointLight (Colour.White, 0.5, Point(-4.0, 2.0, 4.0))
      let ambientLight = AmbientLight(Colour.White, 0.1)
      let (lights:Light list) = [light]
      Scene (s, c, lights, ambientLight, 8)

    // colours
    let aqua = Colour (Color.Aqua)
    let white = Colour (Color.White)

    // materials
    let matteRed = MatteMaterial(Colour.Red)
    let matteGreen = MatteMaterial(Colour.Green)
    let matteYellow = MatteMaterial(Colour(1.,1.,0.))
    let matteWhite = MatteMaterial(Colour.White)
    let matteBlue = MatteMaterial(Colour.Blue)
    let matteGold = MatteMaterial(Colour(Color.Gold))
    let phongYellow = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour(1.,1.,0.))
    let phongRed = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.Red)
    let phongGreen = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.Green)
    let emissive = EmissiveMaterial(Colour.White, 1.)
    
    // helper functions
    let mkShape (bs:baseShape) m = bs.toShape m
    let texfun m =
        let f a b = m
        Texture f

    // shapes
    let sphere1 = mkShape (mkImplicit "x^2 + y^2 + z^2 - 1.0") (texfun matteGreen)
    let sphere2 = mkShape (mkImplicit "(x^2 + y^2 + z^2)_2 - 1.0") (texfun matteRed)
    let torus = mkShape (mkImplicit "(((x^2 + y^2)_2 - 1.5)^2 + z^2)_2 - 0.5") (texfun phongGreen)
    let test1 = mkShape (mkImplicit "(x - 2)^2(x+2)^2 + (y - 2)^2(y+2)^2 + (z - 2)^2(z+2)^2 + 3(x^2*y^2 + x^2z^2 + y^2z^2) + 6x y z - 10(x^2 + y^2 + z^2) + 22") (texfun matteGold)
    let heart = mkShape (mkImplicit "(x^2 + (4.0/9.0)*y^2 + z^2 - 1)^3 - x^2 * z^3 - (9.0/80.0)*y^2*z^3") (texfun matteYellow)
    let torus2 =
      let rs1 = "(1.5^2 + 0.5^2)"
      let rs2 = "(1.5^2 + 0.5^2)"
      let sx = "x^4 + 2x^2*y^2 + 2x^2*z^2 - 2*" + rs1 + "*x^2"
      let sy = "y^4 + 2y^2*z^2 + 2*" + rs2 + "*y^2"
      let sz = "z^4 - 2*" + rs1 + "*z^2"
      let sc = rs2 + "^2"
      let eqn = sx + " + " + sy + " + " + sz + " + " + sc 
      mkShape (mkImplicit eqn) (texfun matteBlue)

    // camera
    let setup =
      let s = [| torus |]
      let camera = PinholeCamera (Point(0.0, 0.0, 4.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 4.0, 500, 500)
      mkScene' s camera
    
    let sc = setup
    sc.RenderParallel |> ignore

    0