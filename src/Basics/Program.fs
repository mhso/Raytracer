open Tracer.Basics
open Tracer.Sampling.Sampling
open System.IO

[<EntryPoint>]
let main _ = 
    
    let position = Point(0.,0.,5.)
    let lookat = Point(0.,0.,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let resX = 1920
    let resY = 1080
    let width = 2.
    let height = (float(resY) / float(resX)) * width
    
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
    
    let sL = SphereShape(Point(-1., 0., -1.), 1., matteRed)
    let sC = SphereShape(Point(0., 0., 0.), 1., matteYellow)
    let sR = SphereShape(Point(1., 0., 1.), 1., matteGreen)
    let sTop = SphereShape(Point(0., 0., 15.), 5., matteWhite)
    let discC = Disc(Point(0., 0., 0.), 4., emissive)
    let rectC = Rectangle(Point(-4., -4., 0.), Point(-4., 4., 0.), Point(4., -4., 0.), emissive)

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 129
    let VIEW_SAMPLES = 8
    let LENS_SAMPLES = 8

    //- CAMERA
    //let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY)
    let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 1.0, 5.0,
                            new SampleGenerator(multiJittered, VIEW_SAMPLES, CAM_SETS),
                            new SampleGenerator(multiJittered, LENS_SAMPLES, CAM_SETS))
    
    //- LIGHTS
    let lightFront     = PointLight(Colour.White, 1.5, Point(0.,0.,7.))
    let lightTop       = DirectionalLight(Colour.White, 1., Vector(0.,-1.,0.))
  
    //- LIGHTS
    let lightRight     = PointLight(Colour.White, 1., Point(0., -30., 0.))
    let lightAmbient   = AmbientLight(Colour.White, 0.1)
    let lightSphere    = SphereAreaLight(emissive, sC, 100, 5)
    let lightDisc      = DiscAreaLight(emissive, discC, 100, 5)
    let lightRect      = RectangleAreaLight(emissive, rectC, 100, 5)
    let plane          = InfinitePlane(matteWhite)

    //- FINAL
    let lights: Light list      = [lightAmbient; lightFront]
    let spheres: Shape list     = [sL;sC;sR]

    let scene                   = Scene(spheres, camera, lights)

    ignore scene.Render
    
    0