open Tracer.Basics
open Tracer.Sampling.Sampling
open System.IO

[<EntryPoint>]
let main _ = 
    
    let position = Point(70.,-70.,7.)
    let lookat = Point(0.,0.,6.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let width = 500.
    let height = 400.
    let resX = 500
    let resY = 400
    
    //- MATERIALS
    let matteRed = MatteMaterial(Colour.Red)
    let matteGreen = MatteMaterial(Colour.Green)
    let matteYellow = MatteMaterial(Colour(1.,1.,0.))
    let matteWhite = MatteMaterial(Colour.White)
    let matteBlue = MatteMaterial(Colour.Blue)
    let phongShades = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.White)
    let perfectWhite = PerfectReflectionMaterial(5, matteWhite, Colour.White, 1.)
    let perfectGreen = PerfectReflectionMaterial(5, matteGreen, Colour.White, 1.)
    let perfectRed = PerfectReflectionMaterial(5, matteRed, Colour.White, 1.)
    let perfectYellow = PerfectReflectionMaterial(5, matteYellow, Colour.White, 1.)
    let glossyWhite = GlossyMaterial(5., Colour.White, matteWhite, 10, 1, 1, 100.)
    let emissive = EmissiveMaterial(Colour.White, 10000.)

    //- SHAPES
    let sphereRed        = SphereShape(Point(-5.,0.,2.), 0.5, matteRed)
    let spherePerfectYellow     = SphereShape(Point(-2.,0.,0.), 0.5, matteYellow)
    let sphereGreen      = SphereShape(Point(1.,0.,-2.), 0.5, matteGreen)
    
    let sL = SphereShape(Point(0., -4., 0.), 0.25, matteRed)
    let sC = SphereShape(Point(-6., 0., 10.), 0.01, emissive)
    let sTop = SphereShape(Point(0., 0., 15.), 5., matteWhite)
    let discC = Disc(Point(0., 0., 0.), 4., emissive)
    let rectC = Rectangle(Point(-4., -4., 0.), Point(-4., 4., 0.), Point(4., -4., 0.), emissive)
    let sR = SphereShape(Point(1., 0., 8.), 1., matteGreen)

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 1
    let VIEW_SAMPLES = 16
    let LENS_SAMPLES = 16

    //- CAMERA
    let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY)
  
    //- LIGHTS
    let lightFront     = PointLight(Colour.White, 1.5, Point(8.,-4.,0.))
    let lightTop       = DirectionalLight(Colour.White, 1., Vector(0.,0.,1.))
    let lightRight     = PointLight(Colour.White, 1., Point(0., -30., 0.))
    let lightAmbient   = AmbientLight(Colour.White, 0.1)
    let lightSphere    = SphereAreaLight(emissive, sC, 100, 5)
    let lightDisc      = DiscAreaLight(emissive, discC, 100, 5)
    let lightRect      = RectangleAreaLight(emissive, rectC, 100, 5)
    let plane          = InfinitePlane(matteWhite)

    //- FINAL
    let lights: Light list      = [lightAmbient; lightRect]
    let spheres: Shape list     = [sR; rectC; sTop]

    let scene                   = Scene(spheres, camera, lights)

    ignore scene.Render
    
    0