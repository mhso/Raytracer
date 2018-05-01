open Tracer.Basics
open Tracer.Sampling.Sampling
open System.IO

[<EntryPoint>]
let main _ = 
    
    let position = Point(7.,2.,2.)
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

    let cylinderOrigin = new Point(0., 0., 0.)
    let radius = 0.5
    let height = 2.
    let Material = new MatteMaterial(new Colour(0., 0., 1.))
    let MaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let cylinder = new HollowCylinder(cylinderOrigin, radius, height, Material)

    let sphereOrigin = new Point(0., 0., 0.)
    let sphereRadius = 1.
    let sphereMat = new MatteMaterial(new Colour(1., 1., 0.))
    let sphereMaterialSpecular = new SpecularMaterial(1., new Colour(1., 0., 1.), 10., new Colour(0., 0., 1.))
    let GlossySphereMat = GlossyMaterial(5., Colour.Red, matteWhite, 10, 1, 1, 10.)
    let sphere = new SphereShape(sphereOrigin, sphereRadius, sphereMat)

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
    let box = new Box(low, high, boxMaterial, boxMaterial2, boxMaterial3, boxMaterial4, boxMaterial5, boxMaterial6)

    let planeMaterial = new MatteMaterial(new Colour(0., 1., 0.4))
    let GlossyPlaneMat = GlossyMaterial(5., Colour.Blue, matteWhite, 10, 1, 1, 5.)
    let infinitePlane = InfinitePlane(planeMaterial)

    let bLeft = new Point(0., 0., 0.)
    let tLeft = new Point(0., 1., 0.)
    let bRight = new Point(1., 0., 0.)
    let rectangleMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let rectangle = Rectangle(bLeft, tLeft, bRight, rectangleMaterial) 

    let discCenter = new Point(0., 0., 0.)
    let discRadius = 1.
    let discMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let disc = Disc(discCenter, discRadius, discMaterial)

    let a = new Point(0., 0., 0.)
    let b = new Point(0., 1., 0.)
    let c = new Point(1., 0., 0.)
    let triangleMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let triangle = Triangle(a, b, c, triangleMaterial)

    let csgShape = CSG(sphere, box, Intersection)

    let shapes : Shape list = [csgShape]

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
    let scene                   = Scene(shapes, camera, lights)

    ignore scene.Render
    
    0