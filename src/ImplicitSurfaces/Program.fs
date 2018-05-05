namespace Tracer.ImplicitSurfaces

module Program =

  open System.Drawing
  open Tracer.Basics
  open Tracer.ImplicitSurfaces.Main
  open Tracer.Basics

  type baseShape = Main.baseShape
  type shape = Main.shape

  [<EntryPoint>]
  let main _ = 

    let mkScene' s (c:Camera) =
      let light = PointLight (Colour.White, 0.5, Point(4.0, 2.0, 4.0))
      let light2 = PointLight (Colour.White, 0.5, Point(-4.0, 2.0, 4.0))
      let ambientLight = AmbientLight(Colour.White, 0.1)
      let (lights:Light list) = [light; light2; ambientLight]
      Scene (s, c, lights)

    //- MATERIALS
    let matteRed = MatteMaterial(Colour.Red)
    let matteGreen = MatteMaterial(Colour.Green)
    let matteYellow = MatteMaterial(Colour(1.,1.,0.))
    let matteWhite = MatteMaterial(Colour.White)
    let matteBlue = MatteMaterial(Colour.Blue)
    let phongYellow = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour(1.,1.,0.))
    let phongRed = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.Red)
    let phongGreen = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.Green)
    let perfectWhite = PerfectReflectionMaterial(5, matteWhite, Colour.White, 1.)
    let perfectGreen = PerfectReflectionMaterial(5, matteGreen, Colour.White, 1.)
    let perfectRed = PerfectReflectionMaterial(5, matteRed, Colour.White, 1.)
    let perfectYellow = PerfectReflectionMaterial(5, matteYellow, Colour.White, 1.)
    let glossyWhite = GlossyMaterial(5., Colour.White, matteWhite, 6, 1, 1, 10.)
    let emissive = EmissiveMaterial(Colour.White, 1.)

    let mkShape (bs:baseShape) m = bs.toShape m

    let texfun m =
        let f a b = m
        Texture f

    let sphere1 (r : float) =
      let aqua = Colour (Color.Aqua)
      let white = Colour (Color.White)

      let s = [|mkShape (mkImplicit ("x^2 + (y)^2 + z^2 - " + (string (r * r)))) (texfun (SpecularMaterial (0.5, aqua, 0.7, white)));
               mkShape (mkImplicit ("(x + 3)^2 + (y - 2)^2 + z^2 - " + (string (r * r)))) (texfun (SpecularMaterial (0.5, Colour.Green, 0.7, white)));
               mkShape (mkImplicit ("(x - 3)^2 + (y)^2 + z^2 - " + (string (r * r)))) (texfun (SpecularMaterial (0.5, Colour.Red, 0.7, white)));
               //(InfinitePlane (texfun perfectWhite) :> Shape)
               //mkShape (implicitPlane "y") (texfun phongGreen)
               |]
      let camera = PinholeCamera (Point(0.0, 1.0, 4.4), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 3.0, 1024, 768)
      mkScene' s camera

    let sc = sphere1 1.
    sc.RenderParallel |> ignore

    0