﻿namespace Tracer.ImplicitSurfaces

module Program =

  open System.Drawing
  open Tracer.Basics
  open Tracer.Sampling.Sampling
  open Tracer.ImplicitSurfaces.Main
  open Tracer.Basics.Render
  open Tracer.Basics.Acceleration
  open Tracer.Basics.Textures
  open Tracer.Basics.Transform
  open Tracer.Basics.Transformation
  open System

  type baseShape = Tracer.BaseShape.BaseShape
  type shape = Tracer.Basics.Shape

  [<EntryPoint>]
  let main _ = 

    setAcceleration KDTree

    // Scene
    let mkScene' s =
      let light = PointLight (Colour.White, 0.5, Point(4.0, 2.0, 4.0))
      let light2 = PointLight (Colour.White, 0.5, Point(-4.0, 2.0, 4.0))
      let ambientLight = AmbientLight(Colour.White, 0.1)
      let (lights:Light list) = [light;light2]
      Scene ([s], lights, ambientLight, 0)

    // Colours
    let aqua = Colour (Color.Aqua)
    let white = Colour (Color.White)
    
    // Helper functions
    let mkshape (bs:baseShape) t = bs.toShape t

    // Shapes, cams, and colours
    let sphere1mat = PhongMaterial (aqua, 0.2, aqua, 0.8, white, 0.7, 100)
    let sphere1 = mkshape (mkImplicit "x^2 + y^2 + z^2 - 1.0") (mkMatTexture sphere1mat)
    let sphere1 = SphereShape(Point.Zero, 1.0, mkMatTexture sphere1mat)
    let sphere1cam = PinholeCamera (Point(0.0, 0.0, 4.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 3.0, 1024, 768, multiJittered 4 87)

    let sphere2mat = MatteMaterial (Colour.Blue, 1.0, Colour.Blue, 1.0)
    let sphere2 = mkshape (mkImplicit "(x^2 + y^2 + z^2)_2 - 1.0") (mkMatTexture sphere2mat)
    let sphere2cam = PinholeCamera (Point(0.0, 0.0, 4.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 4.0, 500, 500, regular 1)
    
    let torus = mkshape (mkImplicit "(((x^2 + y^2)_2 - 1.5)^2 + z^2)_2 - 0.5") (mkMatTexture sphere2mat)
    let toruscam = PinholeCamera (Point(0.0, 0.0, 4.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 4.0, 500, 500, regular 1)
    
    let linktorus1mat = mkMatTexture (PhongReflectiveMaterial (Colour(Color.Orange), 0.2, Colour(Color.Orange), 0.8, Colour.White, 0.3, Colour.White, 1.0, 100))
    let linktorus2mat = mkMatTexture (PhongReflectiveMaterial (Colour(Color.Violet), 0.2, Colour(Color.Violet), 0.8, Colour.White, 0.3, Colour.White, 1.0, 100))
    let linktorus =
      let bs = mkImplicit ("(((x^2 + y^2)_2 - " + (string 2.0) + ")^2 + z^2)_2 - " + (string 0.3))
      let t1 = transform (bs.toShape linktorus1mat) (translate -1.5 0.0 0.0)
      let t2 = transform (bs.toShape linktorus2mat) (mergeTransformations [rotateX 90.0; translate 1.5 0.0 0.0])
      new CSG(t1, t2, CSGOperator.Grouping) :> shape
    let linktoruscam = PinholeCamera (Point (0.0, 0.0, 6.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 3.0, 1024, 768, multiJittered 4 83)

    let torus2 =
      let rs1 = "(1.5^2 + 0.5^2)"
      let rs2 = "(1.5^2 - 0.5^2)"
      let sx = "x^4 + 2x^2*y^2 + 2x^2*z^2 - 2*" + rs1 + "*x^2"
      let sy = "y^4 + 2y^2*z^2 + 2*" + rs2 + "*y^2"
      let sz = "z^4 - 2*" + rs1 + "*z^2"
      let sc = rs2 + "^2"
      let eqn = sx + " + " + sy + " + " + sz + " + " + sc 
      //let eqn = "x^4+2x^2*y^2+2x^2*z^2+(-5x)^2+y^4+2y^2*z^2+4y^2+z^4+(-5z^2)+4"
      mkshape (mkImplicit eqn) (mkMatTexture sphere2mat)
    let torus2cam = PinholeCamera (Point(0.0, 4.0, 0.0), Point(0.0, 0.0, 0.0), Vector(0.0, 0.0, 1.0), 2.0, 4.0, 4.0, 500, 500, regular 1)

    let testshapemat = MatteMaterial (Colour(Color.Gold), 1.0, Colour(Color.Gold), 1.0)
    let testshape = mkshape (mkImplicit "(x - 2)^2*(x+2)^2 + (y - 2)^2(y+2)^2 + (z - 2)^2(z+2)^2 + 3(x^2*y^2 + x^2z^2 + y^2z^2) + 6x y z - 10(x^2 + y^2 + z^2) + 22") (mkMatTexture testshapemat)
    let testshapecam = PinholeCamera (Point(6.0, 6.0, 8.0), Point(0.0, 0.0, 0.0), Vector(-1.0, -1.0, 0.0), 2.0, 4.0, 4.0, 500, 500, regular 1)
    
    let heartmat = MatteMaterial (Colour(Color.DarkRed), 1.0, Colour(Color.DarkRed), 1.0)
    let heart = mkshape (mkImplicit "(x^2 + (4.0/9.0)*(y+1)^2 + z^2 - 1)^3 - x^2 * z^3 - (9.0/80.0)*(y+1)^2*z^3") (mkMatTexture heartmat)
    let heartcam = PinholeCamera (Point(0.0, 3.0, 1.0), Point(0.0, 0.0, 0.0), Vector(0.0, 0.0, 1.0), 2.0, 4.0, 4.0, 500, 500, regular 1)

    let factorial x = 
      if x = 0 then 1 else
      let rec fac_aux a acc =
        if a >= x then
          a * acc
        else
          fac_aux (a + 1) (a * acc)
      fac_aux 1 x

    let comb a b = 
      let x = float (factorial a) in
      let y = float (factorial b) in
      let z = float (factorial (a - b)) in
        x / (y * z)

    let rec strSum n f : string =
      if n = 0 then
        f 0
      else
        f n + " + " + (strSum (n - 1) f)

    let chmutov degree =       
      let T x = strSum (degree / 2) (fun (k : int) -> (string (comb degree (2 * k))) + " * (" + x + "^2 + -1.0)^" + (string k) + " * " + x + "^" + (string (degree - (2 * k))))
      let is = mkImplicit (T "x" + " + " + T "y" + " + " + T "z")
      let s = mkshape is (mkMatTexture testshapemat)
      s
    let chmutovcam = PinholeCamera (Point (16.0, 16.0, 16.0), Point (0.0, -0.5, 0.0), Vector (-1.0, 1.0, -1.0), 16.0, 4.0, 4.0, 500, 500, regular 1)

    //let render = Render(mkScene' sphere1, sphere1cam)
    //let render = Render(mkScene' sphere2, sphere2cam)
    //let render = Render(mkScene' torus, toruscam)
    let render = Render(mkScene' heart, heartcam)
    //let render = Render(mkScene' (chmutov 2), chmutovcam)
    //let render = Render(mkScene' (chmutov 3), chmutovcam)
    //let render = Render(mkScene' (chmutov 4), chmutovcam)
    //let render = Render(mkScene' (chmutov 5), chmutovcam)
    //let render = Render(mkScene' (chmutov 6), chmutovcam)
    //let render = Render(mkScene' torus2, torus2cam)
    //let render = Render(mkScene' testshape, testshapecam)
    //let render = Render(mkScene' linktorus, linktoruscam)

    render.RenderToScreen render.RenderParallel |> ignore
    Console.ReadKey () |> ignore
    0