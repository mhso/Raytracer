open Tracer.Basics
open Tracer.Sampling.Sampling
open System.IO

[<EntryPoint>]
let main _ = 
    
    let position = Point(7.,0.,0.)
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
    let phongShades = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.White)
    let perfectWhite = PerfectReflectionMaterial(5, matteWhite, Colour.White, 1.)
    let perfectGreen = PerfectReflectionMaterial(5, matteGreen, Colour.White, 1.)
    let perfectRed = PerfectReflectionMaterial(5, matteRed, Colour.White, 1.)
    let perfectYellow = PerfectReflectionMaterial(5, matteYellow, Colour.White, 1.)
    let glossyWhite = GlossyMaterial(5., Colour.White, matteWhite, 10, 1, 1, 100.)
    let emissive = EmissiveMaterial(Colour.White, 1.)

    //- SHAPES
    let sphereRed        = SphereShape(Point(-5.,0.,2.), 0.5, matteRed)
    let spherePerfectYellow     = SphereShape(Point(-2.,0.,0.), 0.5, matteYellow)
    let sphereGreen      = SphereShape(Point(1.,0.,-2.), 0.5, matteGreen)
    
    let sL = SphereShape(Point(0., 0., -2.), 1., matteRed)
    let sC = SphereShape(Point(0., 0., 0.), 1., matteRed)
    let sR = SphereShape(Point(0., 0., 2.), 1., matteRed)

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 29
    let VIEW_SAMPLES = 8
    let DISC_SAMPLES = 8

    //- CAMERA
    let camera         = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY)
    
    //- LIGHTS
    let lightFront     = PointLight(Colour.White, 1.5, Point(8.,-4.,0.))
    let lightTop       = DirectionalLight(Colour.White, 1., Vector(0.,-1.,0.))
    let lightAmbient   = AmbientLight(Colour.White, 0.1)

    //- FINAL
    let lights: Light list      = [lightAmbient; lightTop]
    let spheres: Shape list     = [sL;sC;sR]
    let scene                   = Scene(spheres, camera, lights)

    ignore scene.Render
    
    0