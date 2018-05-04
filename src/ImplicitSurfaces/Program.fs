namespace Tracer.ImplicitSurfaces

module Program =

  open System.Drawing
  open Tracer.Basics
  open Tracer.ImplicitSurfaces.Main

  type baseShape = Main.baseShape
  type shape = Main.shape

  [<EntryPoint>]
  let main _ = 

    let position = Point(1., 1., 4.)
    let lookat = Point(0.,0.,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let resX = 640
    let resY = 640
    let width = 2.
    let height = 2.
    //let height = (float resY / float resX) * width

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

      let s = [|mkShape (mkImplicit ("x^2 + y^2 + z^2 - " + (string (r * r)))) (texfun (SpecularMaterial (0.2, aqua, 0.7, white)));
               mkShape (mkImplicit ("(x + 3)^2 + y^2 + z^2 - " + (string (r * r)))) (texfun (SpecularMaterial (0.5, aqua, 0.7, white)));
               mkShape (mkImplicit ("(x - 3)^2 + y^2 + z^2 - " + (string (r * r)))) (texfun (SpecularMaterial (0.5, aqua, 0.7, white)));
               //mkShape (mkImplicit "y") matteYellow
               mkShape (implicitPlane "y") (texfun phongGreen)
               |]
      let camera = PinholeCamera (Point(0.0, 2.0, 6.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 3.0, 1024, 768)
      mkScene' s camera

    let sc = sphere1 1.
    sc.RenderParallel |> ignore

    //- SHAPES
    let sphere1 = mkShape (mkImplicit ("x^2 + y^2 + z^2 - " + (string (1.**2.0)))) (texfun phongGreen)
    let sphere2 = mkShape (mkImplicit ("(x - 3)^2 + (y)^2 + z^2 - " + (string (1.0**2.0)))) (texfun phongGreen)
    let sphere3 = mkShape (mkImplicit ("(x + 3)^2 + (y)^2 + z^2 - " + (string (1.0**2.0)))) (texfun phongGreen)
    let sphere4 = mkShape (mkImplicit ("(x)^2 + (y - 3)^2 + z^2 - " + (string (1.0**2.0)))) (texfun phongGreen)
    let sphere5 = mkShape (mkImplicit ("(x)^2 + (y + 3)^2 + z^2 - " + (string (1.0**2.0)))) (texfun phongGreen)

    let plane = mkShape (mkImplicit "z") (texfun phongGreen)

    let low = new Point(0., 0., 0.)
    let high = new Point(1., 1., 1.)
    let boxMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let boxMaterial2 = new MatteMaterial(new Colour(0., 0., 1.))
    let boxMaterial3 = new MatteMaterial(new Colour(1., 0., 1.))
    let boxMaterial4 = new MatteMaterial(new Colour(1., 1., 0.))
    let boxMaterial5 = new MatteMaterial(new Colour(0.5, 0.5, 1.))
    let boxMaterial6 = new MatteMaterial(new Colour(0.3, 0., 0.6))
    let GlossyBoxMat = GlossyMaterial(5., Colour.Red, matteWhite, 10, 1, 1, 5.)
    let sphereMaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    //let box = new Box(low, high, boxMaterial, boxMaterial2, boxMaterial3, boxMaterial4, boxMaterial5, boxMaterial6)

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 1
    let VIEW_SAMPLES = 16
    let LENS_SAMPLES = 16

    //- CAMERA
    let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY)
    //let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 6.0, 4.0,
    //                        new SampleGenerator(multiJittered, VIEW_SAMPLES, CAM_SETS),
    //                        new SampleGenerator(multiJittered, LENS_SAMPLES, CAM_SETS))
    
    //- LIGHTS
    let lightFront     = PointLight(Colour.White, 1.5, Point(8.,-4.,0.))
    let lightTop       = DirectionalLight(Colour.White, 1., Vector(0.,-1.,0.))
    let lightAmbient   = AmbientLight(Colour.White, 0.1)

    //- FINAL
    let lights: Light list      = [lightAmbient; lightTop]
<<<<<<< HEAD
    let spheres: Shape array     = [|sphere1|]
    let scene                   = Scene(spheres, camera, lights)
=======
    let spheres: Shape []       = [|sphere1; sphere2; sphere3; sphere4;sphere5|]
    let scene                   = Scene (spheres, camera, lights)
>>>>>>> d62bf63dba4f80b73c3fd6c4e9808480bca05e93

    //scene.Render |> ignore

    0