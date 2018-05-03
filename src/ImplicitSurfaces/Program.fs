namespace Tracer.ImplicitSurfaces

module Program =

  open Tracer.Basics
  open Tracer.ImplicitSurfaces.Main

  type baseShape = Tracer.ImplicitSurfaces.Main.baseShape
  type shape = Tracer.ImplicitSurfaces.Main.shape

  [<EntryPoint>]
  let main _ = 

    let position = Point(17.,0.0,1.)
    let lookat = Point(0.,0.,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let width = 1920.
    let height = 1080.
    let resX = 1920
    let resY = 1080
    
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

    //- SHAPES
    let sphere1 = mkShape (mkImplicit ("x^2 + y^2 + z^2 - " + (string (1.**2.0)))) phongYellow
    let sphere2 = mkShape (mkImplicit ("(x-2)^2 + (y+1)^2 + (z-1)^2 - " + (string (2.0**2.0)))) phongRed
    let sphere3 = mkShape (mkImplicit ("(x+3)^2 + (y-3)^2 + (z+2)^2 - " + (string (4.0**2.0)))) phongGreen

    let plane = mkShape (mkImplicit "z") matteWhite

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
    let spheres: Shape list     = [sphere1; sphere2; sphere3]
    let scene                   = Scene(spheres, camera, lights)

    scene.Render |> ignore

    0