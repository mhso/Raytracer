open Tracer.Basics.Textures
open Tracer.Sampling.Sampling
open System.IO
open Tracer.Basics
open Transformation
open Tracer.Sampling
open System.Diagnostics

[<EntryPoint>]
let main _ = 
    
    let position = Point(-5.,3.,0.)
    let lookat = Point(0.5,1.,0.5)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let resX = 640
    let resY = 480
    let width = 2.
    let height = (float(resY) / float(resX)) * width
    
    //- MATERIALS
    let matteRed = MatteMaterial(Colour.Red)
    let matteGreen = MatteMaterial(Colour.Green)
    let matteYellow = MatteMaterial(Colour(1.,1.,0.))
    let matteWhite = MatteMaterial(Colour.White)
    let matteGray = MatteMaterial(Colour(0.7, 0.7, 0.7))
    let matteBlue = MatteMaterial(Colour.Blue)
    let phongShades = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.White)
    let perfectWhite = PerfectReflectionMaterial(5, matteWhite, Colour.White, 1.)
    let perfectGreen = PerfectReflectionMaterial(5, matteGreen, Colour.White, 1.)
    let perfectRed = PerfectReflectionMaterial(5, matteRed, Colour.White, 1.)
    let perfectYellow = PerfectReflectionMaterial(5, matteYellow, Colour.White, 1.)
    let glossyWhite = GlossyMaterial(5., Colour.White, matteWhite, 10, 1, 1, 100.)
    let emissive = EmissiveMaterial(Colour.White, 1.)

    //- SHAPES
    let sphereRed        = SphereShape(Point(-5.,0.,2.), 0.5, mkMatTexture matteRed)
    let spherePerfectYellow     = SphereShape(Point(-2.,0.,0.), 0.5, mkMatTexture perfectWhite)
    let sphereGreen      = SphereShape(Point(1.,0.,-2.), 0.5, mkMatTexture matteGreen)
    
    let sLr = SphereShape(Point(0., 0., 0.), 1., mkMatTexture matteRed)
    let sCr = SphereShape(Point(0., 0., 0.), 3., mkMatTexture matteGray)
    let sRr = SphereShape(Point(0., 0., 0.), 1., mkMatTexture matteGreen)

    let boxTex = mkMatTexture phongShades
    let rLr = Box(Point(0., 0., 0.), Point(1., 2., 1.), boxTex, boxTex, boxTex, boxTex, boxTex, boxTex)

    
    let rL = Transform.transform rLr (translate 0. 0. -3.)
    let rC = Transform.transform rLr (translate 0. 1. -1.)
    let rR = Transform.transform rLr (translate 0. 1. 1.)

    let sL = Transform.transform sLr (translate 0. 1. 0.)
    let sC = Transform.transform sCr (translate 0. -3. 0.)
    let sR = Transform.transform sRr (translate 0. 1. 2.5)
    
    let sTop = SphereShape(Point(0., 0., 15.), 5., mkMatTexture matteWhite)
    let discC = Disc(Point(0., 0., 0.), 4., mkMatTexture emissive)
    let rectC = Rectangle(Point(-4., -4., 0.), Point(-4., 4., 0.), Point(4., -4., 0.), mkMatTexture emissive)

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 129
    let VIEW_SAMPLES = 8
    let LENS_SAMPLES = 8

    //- CAMERA
    let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY)
    //let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 1.0, 5.0,
    //                        new SampleGenerator(multiJittered, VIEW_SAMPLES, CAM_SETS),
    //                        new SampleGenerator(multiJittered, LENS_SAMPLES, CAM_SETS))
    
    //- LIGHTS
    let lightFront     = PointLight(Colour.White, 1.5, Point(7.,7.,7.))
    let lightTop       = DirectionalLight(Colour.White, 1., Vector(0.,1.,0.))
    
    let lightEnviro     = EnvironmentLight(10000., mkMatTexture emissive, Sampling.SampleGenerator(Sampling.multiJittered, 5, 1))
    let ambientOccluder = AmbientOccluder(1., Colour.White, 0., Sampling.SampleGenerator(Sampling.multiJittered, 101, 1))
    let planeR = InfinitePlane(mkMatTexture matteWhite)

    //- LIGHTS
    //let lightRight     = PointLight(Colour.White, 1., Point(0., -30., 0.))
    //let lightAmbient   = AmbientLight(Colour.White, 0.1)
    //let lightDisc      = DiscAreaLight(emissive, discC, 100, 5)
    //let lightRect      = RectangleAreaLight(emissive, rectC, 100, 5) 

    //- FINAL
    let lights: Light list      = [ambientOccluder]
    let spheres: Shape []       = [| sL; sC |]
    
    let scene                   = Scene(spheres, camera, lights)

    ignore scene.RenderParallel
    
    0