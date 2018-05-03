open Tracer.Basics
open Tracer.Sampling.Sampling
open System.IO

[<EntryPoint>]
let main _ = 
    
    let position = Point(7.,-4.,-4.)
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
    
    let sL = SphereShape(Point(0., 0., 0.), 1., matteRed)
    let sC = SphereShape(Point(-4., 0., 0.), 1., glossyWhite)
    let sR = SphereShape(Point(-8., 0., -3.), 1., matteGreen)
    let plane = InfinitePlane(glossyWhite)

    let cylinderOrigin = new Point(0., 0., 0.)
    let radius = 0.5
    let cylinderHeight = 2.
    let Material = new MatteMaterial(new Colour(0., 0., 1.))
    let MaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let cylinder = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, Material)

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
    //let box = new Box(low, high, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat)

    let vlow = new Point(0., 0., 0.)
    let vhigh = new Point(2., 2., 2.)
    let vboxMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let vboxMaterial2 = new MatteMaterial(new Colour(0., 0., 1.))
    let vboxMaterial3 = new MatteMaterial(new Colour(1., 0., 1.))
    let vboxMaterial4 = new MatteMaterial(new Colour(1., 1., 0.))
    let vboxMaterial5 = new MatteMaterial(new Colour(0.5, 0.5, 1.))
    let vboxMaterial6 = new MatteMaterial(new Colour(0.3, 0., 0.6))
    let vGlossyBoxMat = GlossyMaterial(5., Colour.Red, matteWhite, 10, 1, 1, 5.)
    let vsphereMaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let vbox = new Box(vlow, vhigh, vboxMaterial, vboxMaterial2, vboxMaterial3, vboxMaterial4, vboxMaterial5, vboxMaterial6)
    //let box = new Box(low, high, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat)

    let planeMat = new MatteMaterial(new Colour(0., 1., 0.4))
    let GlossyPlaneMat = GlossyMaterial(5., Colour.Blue, matteWhite, 10, 1, 1, 5.)
    let infinitePlane = InfinitePlane(GlossyPlaneMat)

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

    let solidOrigin = new Point(0., 0., 0.)
    let solidRadius = 0.5
    let solidHeight = 2.
    let solidMaterial = new MatteMaterial(new Colour(0., 0.3, 0.8))
    let solidMaterial2 = new MatteMaterial(new Colour(0.5, 0., 1.))
    let solidMaterial3 = new MatteMaterial(new Colour(0., 0.5, 1.))
    let solidMaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let solidCylinder = SolidCylinder(solidOrigin, solidRadius, solidHeight, solidMaterial, solidMaterial2, solidMaterial3)

    let csgShape = CSG(sphere, box, Intersection)
    let csgShape2 = CSG(csgShape, solidCylinder, Union)
    let csgShape3 = CSG(box, csgShape2, Intersection)

    let shapes : Shape list = [box]

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 129
    let VIEW_SAMPLES = 8
    let LENS_SAMPLES = 8

    //- CAMERA
    let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY)
    //let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 4.0, 3.0,
    //                        new SampleGenerator(multiJittered, VIEW_SAMPLES, CAM_SETS),
    //                        new SampleGenerator(multiJittered, LENS_SAMPLES, CAM_SETS))
    
    //- LIGHTS
    let lightFront     = PointLight(Colour.White, 1.5, Point(0.,0.,7.))
    let lightTop       = DirectionalLight(Colour.White, 1., Vector(0.,-1.,0.))
  
    //- LIGHTS
    let lightRight     = PointLight(Colour.White, 1., Point(0., -30., 0.))

    let lightAmbient   = AmbientLight(Colour.White, 0.1)
    let lightSphere    = SphereAreaLight(emissive, sC, 100, 5)
    let lightDisc      = DiscAreaLight(emissive, disc, 100, 5)
    let lightRect      = RectangleAreaLight(emissive, rectangle, 100, 5)
    let plane          = InfinitePlane(matteWhite)

    //- FINAL
    let lights: Light list      = [lightAmbient; lightTop]
    let spheres: Shape list     = [sL;sC;sR;plane]
    let scene                   = Scene(shapes, camera, lights)

    ignore scene.Render
    
    0