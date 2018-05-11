open Tracer.Basics
open Tracer.Sampling.Sampling
open System
open Tracer.Basics.Render
open Tracer.Basics.Transform
//open System.Net.Mime.MediaTypeNames

[<EntryPoint>]
let main _ = 
    Acceleration.setAcceleration Acceleration.Acceleration.KDTree
    let position = Point(2.,2.,5.)
    let lookat = Point(0.,0.,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let resX = 1920
    let resY = 1080
    let width = 2.
    let height = (float(resY) / float(resX)) * width
    let maxReflectionBounces = 3
    
    //- MATERIALS
    let matteRed = MatteMaterial(Colour.Red, 1., Colour.Red, 1.)
    let matteGreen = MatteMaterial(Colour.Green, 1., Colour.Green, 1.)
    let matteYellow = MatteMaterial(Colour(1.,1.,0.), 1., Colour(1.,1.,0.), 1.)
    let matteWhite = MatteMaterial(Colour.White, 1., Colour.White, 1.)
    let matteBlue = MatteMaterial(Colour.Blue, 1., Colour.Blue, 1.)

    (*
    let phongShades = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.White)
    let perfectWhite = PerfectReflectionMaterial(matteWhite, Colour.White, 1.)
    let perfectGreen = PerfectReflectionMaterial(matteGreen, Colour.White, 1.)
    let perfectRed = PerfectReflectionMaterial(matteRed, Colour.White, 1.)
    let perfectYellow = PerfectReflectionMaterial(matteYellow, Colour.White, 1.)
    let glossyWhite = GlossyMaterial(5., Colour.White, matteWhite, 10, 1, 100.)
    *)

    let emissive = EmissiveMaterial(Colour.White, 10000.)


    (*
    //- SHAPES
    let sphereRed        = SphereShape(Point(-5.,0.,2.), 0.5, matteRed)
    let spherePerfectYellow     = SphereShape(Point(-2.,0.,0.), 0.5, matteYellow)
    let sphereGreen      = SphereShape(Point(1.,0.,-2.), 0.5, matteGreen)
    
    let sL = SphereShape(Point(0., 0., 0.), 1., matteRed)
    let sC = SphereShape(Point(-4., 0., 0.), 1., glossyWhite)
    let sR = SphereShape(Point(-8., 0., -3.), 1., matteGreen)
    let plane = InfinitePlane(glossyWhite)
    *)

    let cylinderOrigin = new Point(0., 0., 0.)
    let radius = 0.5
    let cylinderHeight = 2.
    let texCylinder = Textures.mkMatTexture(matteBlue)
    let cylinder = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)

    let sphereOrigin = new Point(0., 0., 0.)
    let sphereRadius = 1.
    let texSphere = Textures.mkMatTexture(matteGreen)
    let sphere = new SphereShape(sphereOrigin, sphereRadius, texSphere)

    let low = new Point(0., 0., 0.)
    let high = new Point(1., 1., 1.)
    let boxMaterial = new MatteMaterial(new Colour(0., 1., 1.), 1., Colour(0., 1., 1.), 1.)
    let boxMaterial2 = new MatteMaterial(new Colour(0., 0., 1.), 1., Colour(0., 0., 1.), 1.)
    let boxMaterial3 = new MatteMaterial(new Colour(1., 0., 1.), 1., Colour(1., 0., 1.), 1.)
    let boxMaterial4 = new MatteMaterial(new Colour(1., 1., 0.), 1., Colour(1., 1., 0.), 1.)
    let boxMaterial5 = new MatteMaterial(new Colour(0.5, 0.5, 1.), 1., Colour(0.5, 0.5, 1.), 1.)
    let boxMaterial6 = new MatteMaterial(new Colour(0.3, 0., 0.6), 1., Colour(0.3, 0., 0.6), 1.)
    let texbox = Textures.mkMatTexture(boxMaterial)
    let texbox2 = Textures.mkMatTexture(boxMaterial2)
    let texbox3 = Textures.mkMatTexture(boxMaterial3)
    let texbox4 = Textures.mkMatTexture(boxMaterial4)
    let texbox5 = Textures.mkMatTexture(boxMaterial5)
    let texbox6 = Textures.mkMatTexture(boxMaterial6)
    let box = new Box(low, high, texbox, texbox2, texbox3, texbox4, texbox5, texbox6)
    //let box = new Box(low, high, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat)

    let vlow = new Point(0., 0., 0.)
    let vhigh = new Point(2., 2., 2.)
    let vboxMaterial = new MatteMaterial(new Colour(0., 1., 1.), 1., Colour(0., 1., 1.), 1.)
    let vboxMaterial2 = new MatteMaterial(new Colour(0., 0., 1.), 1., Colour(0., 0., 1.), 1.)
    let vboxMaterial3 = new MatteMaterial(new Colour(1., 0., 1.), 1., Colour(1., 0., 1.), 1.)
    let vboxMaterial4 = new MatteMaterial(new Colour(1., 1., 0.), 1., Colour(1., 1., 0.), 1.)
    let vboxMaterial5 = new MatteMaterial(new Colour(0.5, 0.5, 1.), 1., Colour(0.5, 0.5, 1.), 1.)
    let vboxMaterial6 = new MatteMaterial(new Colour(0.3, 0., 0.6), 1., Colour(0.3, 0., 0.6), 1.)
    //let vbox = new Box(vlow, vhigh, vboxMaterial, vboxMaterial2, vboxMaterial3, vboxMaterial4, vboxMaterial5, vboxMaterial6)
    //let box = new Box(low, high, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat)

    let texPlane = Textures.mkMatTexture(matteRed)
    let infinitePlane = InfinitePlane(texPlane)

    let bLeft = new Point(0., 0., 0.)
    let tLeft = new Point(0., 1., 0.)
    let bRight = new Point(1., 0., 0.)
    let texRectangle = Textures.mkMatTexture(matteWhite)
    let rectangle = Rectangle(bLeft, tLeft, bRight, texRectangle) 

    let discCenter = new Point(0., 0., 0.)
    let discRadius = 1.
    let texDisc = Textures.mkMatTexture(matteBlue)
    let disc = Disc(discCenter, discRadius, texDisc)

    let a = new Point(0., 0., 0.)
    let b = new Point(0., 1., 0.)
    let c = new Point(1., 0., 0.)
    let triangleMaterial = new MatteMaterial(new Colour(0., 1., 1.), 1., new Colour(0., 1., 1.), 1.)
    let texTri = Textures.mkMatTexture(triangleMaterial)
    let triangle = Triangle(a, b, c, new MatteMaterial(new Colour(0., 1., 1.), 1., new Colour(0., 1., 1.), 1.))

    let solidOrigin = new Point(0., 0., 0.)
    let solidRadius = 0.5
    let solidHeight = 2.
    let texSolid = Textures.mkMatTexture(matteGreen)
    let texSolid2 = Textures.mkMatTexture(matteBlue)
    let texSolid3 = Textures.mkMatTexture(matteRed)
    let solidCylinder = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    let solidCylinder2 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    let solidCylinder3 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    let solidCylinder4 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    let solidCylinder5 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    let solidCylinder6 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    let solidCylinder7 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    let solidCylinder8 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    let solidCylinder9 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    let solidCylinder10 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)

    let csgTestInsideEdges = CSG(sphere, box, Union)

    let csgShape = CSG(sphere, box, Intersection)
    let csgShape2 = CSG(csgShape, solidCylinder, Union)
    let csgShape3 = CSG(box, csgShape2, Intersection)

    let csgSub = CSG(box, sphere, Subtraction)


    let move = Transformation.translate 0. -1. 0.
    let transCylinder = Transform.transform cylinder move
    let transCSG = Transform.transform csgShape3 move

    //Fancy CSG
    let texSolidYellow = Textures.mkMatTexture(matteYellow)
    let solid1 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolidYellow, texSolidYellow, texSolidYellow)
    let solid2 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolidYellow, texSolidYellow, texSolidYellow)
    let solid3 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolidYellow, texSolidYellow, texSolidYellow)

    let pi = Math.PI

    let rotateX = Transformation.rotateX (pi/2.)
    let rotateZ = Transformation.rotateZ (pi/2.)

    let transSolid2 = Transform.transform solid2 rotateX
    let transSolid3 = Transform.transform solid3 rotateZ

    let csgUnion1 = CSG(solid1, transSolid2, Union)
    let csgUnion2 = CSG(csgUnion1, transSolid3, Union)

    let boxTex = Textures.mkMatTexture(matteRed)
    let boxForcsg = Box(Point(-0.5,-0.5,-0.5), Point(0.5,0.5,0.5), boxTex, boxTex, boxTex, boxTex, boxTex, boxTex)
    let sphereForcsg = SphereShape(Point(0.,0.,0.), 0.65, Textures.mkMatTexture(matteBlue))

    let csgInter = CSG(boxForcsg, sphereForcsg, Intersection)

    let fancyCSG = CSG(csgInter, csgUnion2, Subtraction)

    //Subtraction Test

    let csgSubTest = CSG(boxForcsg, csgUnion1, Subtraction)

    let shapes : Shape List = [csgSubTest]
    //let shapes : Shape List = [solidCylinder;solidCylinder2;solidCylinder3;solidCylinder4;solidCylinder5;solidCylinder6;solidCylinder7;
                                //solidCylinder8;solidCylinder9;solidCylinder10]
    

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 129
    let VIEW_SAMPLES = 8
    let LENS_SAMPLES = 8

    //- CAMERA
    let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY, multiJittered 1 1)
    //let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 4.0, 3.0,
    //                        new SampleGenerator(multiJittered, VIEW_SAMPLES, CAM_SETS),
    //                        new SampleGenerator(multiJittered, LENS_SAMPLES, CAM_SETS))
    
    //- LIGHTS
    let lightFront     = PointLight(Colour.White, 1.5, Point(2.,2.,7.))
    let lightTop       = DirectionalLight(Colour.White, 1., Vector(1.,3.,0.))
    let lightBack     = PointLight(Colour.White, 1.5, Point(-2.,-2.,-7.))
    let lightRight     = PointLight(Colour.White, 1., Point(0., -30., 0.))

    let lightAmbient   = AmbientLight(Colour.White, 0.3)
    //let lightSphere    = SphereAreaLight(emissive, sC, 100, 5)
    //let lightDisc      = DiscAreaLight(emissive, disc, 100, 5)
    //let lightRect      = RectangleAreaLight(emissive, rectangle, 100, 5)
    //let plane          = InfinitePlane(matteWhite)

    //- FINAL
    let lights: Light list      = [lightFront; lightBack; lightRight]
    //let spheres: Shape list     = [sL;sC;sR;plane]
    let scene                   = Scene(shapes, lights, lightAmbient, 100)

    let render = new Render(scene, camera)

    ignore (render.RenderToFile render.RenderParallel "TracedRays.bmp")

    0

