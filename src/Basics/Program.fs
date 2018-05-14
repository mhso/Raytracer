open Tracer.Basics
open Tracer.Basics.Sampling
open System
open Tracer.Basics.Render
open Tracer.Basics.Transform
open System.Drawing
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.ImplicitSurfaces
open Tracer.Basics.Render
open Tracer.Basics.Acceleration
open Tracer.Basics.Textures
open Tracer.Basics.Transform
open Tracer.Basics.Transformation
open System
//open System.Net.Mime.MediaTypeNames

[<EntryPoint>]
let main _ = 
    //Acceleration.setAcceleration Acceleration.Acceleration.KDTree
    //let position = Point(-2.2,2.2,5.2)
    //let lookat = Point(0.,0.,0.)
    //let up = Vector(0.,1.,0.)
    //let zoom = 1.
    //let resX = 1920
    //let resY = 1080
    //let width = 2.
    //let height = (float(resY) / float(resX)) * width
    //let maxReflectionBounces = 3
    ////let matteGreen = MatteMaterial(Colour.Green, 1., Colour.Green, 1.)
    ////let matteYellow = MatteMaterial(Colour(1.,1.,0.), 1., Colour(1.,1.,0.), 1.)
    ////let matteWhite = MatteMaterial(Colour.White, 1., Colour.White, 1.)
    ////let matteBlue = MatteMaterial(Colour.Blue, 1., Colour.Blue, 1.)
    ////(*
    ////let phongShades = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.White)
    ////let perfectWhite = PerfectReflectionMaterial(matteWhite, Colour.White, 1.)
    ////let perfectGreen = PerfectReflectionMaterial(matteGreen, Colour.White, 1.)
    ////let perfectRed = PerfectReflectionMaterial(matteRed, Colour.White, 1.)
    ////let perfectYellow = PerfectReflectionMaterial(matteYellow, Colour.White, 1.)
    ////let glossyWhite = GlossyMaterial(5., Colour.White, matteWhite, 10, 1, 100.)
    ////*)
    ////*)
    
    ////- MATERIALS
    //let matteRed = MatteMaterial(Colour.Red, 1., Colour.Red, 1.)
    //let matteGreen = MatteMaterial(Colour.Green, 1., Colour.Green, 1.)
    //let matteYellow = MatteMaterial(Colour(1.,1.,0.), 1., Colour(1.,1.,0.), 1.)
    //let matteWhite = MatteMaterial(Colour.White, 1., Colour.White, 1.)
    //let matteBlue = MatteMaterial(Colour.Blue, 1., Colour.Blue, 1.)

    //(*
    //let phongShades = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.White)
    //let perfectWhite = PerfectReflectionMaterial(matteWhite, Colour.White, 1.)
    //let perfectGreen = PerfectReflectionMaterial(matteGreen, Colour.White, 1.)
    //let perfectRed = PerfectReflectionMaterial(matteRed, Colour.White, 1.)
    //let perfectYellow = PerfectReflectionMaterial(matteYellow, Colour.White, 1.)
    //let glossyWhite = GlossyMaterial(5., Colour.White, matteWhite, 10, 1, 100.)
    //*)

    //let emissive = EmissiveMaterial(Colour.White, 10000.)
    ////let boxMaterial = new MatteMaterial(new Colour(0., 1., 1.), 1., Colour(0., 1., 1.), 1.)
    ////let boxMaterial2 = new MatteMaterial(new Colour(0., 0., 1.), 1., Colour(0., 0., 1.), 1.)
    ////let boxMaterial3 = new MatteMaterial(new Colour(1., 0., 1.), 1., Colour(1., 0., 1.), 1.)
    ////let boxMaterial4 = new MatteMaterial(new Colour(1., 1., 0.), 1., Colour(1., 1., 0.), 1.)
    ////let boxMaterial5 = new MatteMaterial(new Colour(0.5, 0.5, 1.), 1., Colour(0.5, 0.5, 1.), 1.)
    ////let boxMaterial6 = new MatteMaterial(new Colour(0.3, 0., 0.6), 1., Colour(0.3, 0., 0.6), 1.)
    ////let texbox = Textures.mkMatTexture(boxMaterial)
    ////let texbox2 = Textures.mkMatTexture(boxMaterial2)
    ////let texbox3 = Textures.mkMatTexture(boxMaterial3)
    ////let texbox4 = Textures.mkMatTexture(boxMaterial4)
    ////let texbox5 = Textures.mkMatTexture(boxMaterial5)
    ////let texbox6 = Textures.mkMatTexture(boxMaterial6)
    ////let box = new Box(low, high, texbox, texbox2, texbox3, texbox4, texbox5, texbox6)
    //////let box = new Box(low, high, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat)

    ////let vboxMaterial4 = new MatteMaterial(new Colour(1., 1., 0.), 1., Colour(1., 1., 0.), 1.)
    ////let vboxMaterial5 = new MatteMaterial(new Colour(0.5, 0.5, 1.), 1., Colour(0.5, 0.5, 1.), 1.)
    ////let vboxMaterial6 = new MatteMaterial(new Colour(0.3, 0., 0.6), 1., Colour(0.3, 0., 0.6), 1.)
    //////let vbox = new Box(vlow, vhigh, vboxMaterial, vboxMaterial2, vboxMaterial3, vboxMaterial4, vboxMaterial5, vboxMaterial6)
    //////let box = new Box(low, high, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat)

    //(*
    ////- SHAPES
    //let sphereRed        = SphereShape(Point(-5.,0.,2.), 0.5, matteRed)
    //let spherePerfectYellow     = SphereShape(Point(-2.,0.,0.), 0.5, matteYellow)
    //let sphereGreen      = SphereShape(Point(1.,0.,-2.), 0.5, matteGreen)
    
    //let sL = SphereShape(Point(0., 0., 0.), 1., matteRed)
    //let sC = SphereShape(Point(-4., 0., 0.), 1., glossyWhite)
    //let sR = SphereShape(Point(-8., 0., -3.), 1., matteGreen)
    //let plane = InfinitePlane(glossyWhite)
    //*)

    //let cylinderOrigin = new Point(0., 0., 0.)
    //let radius = 0.5
    //let cylinderHeight = 2.
    //let texCylinder = Textures.mkMatTexture(matteBlue)
    //let cylinder = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)

    //let sphereOrigin = new Point(0., 0., 0.)
    //let sphereRadius = 1.
    //let texSphere = Textures.mkMatTexture(matteGreen)
    //let sphere = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    ////let texTri = Textures.mkMatTexture(triangleMaterial)
    ////let triangle = Triangle(a, b, c, new MatteMaterial(new Colour(0., 1., 1.), 1., new Colour(0., 1., 1.), 1.))

    //let low = new Point(0., 0., 0.)
    //let high = new Point(1., 1., 1.)
    //let boxMaterial = new MatteMaterial(new Colour(0., 1., 1.), 1., Colour(0., 1., 1.), 1.)
    //let boxMaterial2 = new MatteMaterial(new Colour(0., 0., 1.), 1., Colour(0., 0., 1.), 1.)
    //let boxMaterial3 = new MatteMaterial(new Colour(1., 0., 1.), 1., Colour(1., 0., 1.), 1.)
    //let boxMaterial4 = new MatteMaterial(new Colour(1., 1., 0.), 1., Colour(1., 1., 0.), 1.)
    //let boxMaterial5 = new MatteMaterial(new Colour(0.5, 0.5, 1.), 1., Colour(0.5, 0.5, 1.), 1.)
    //let boxMaterial6 = new MatteMaterial(new Colour(0.3, 0., 0.6), 1., Colour(0.3, 0., 0.6), 1.)
    //let texbox = Textures.mkMatTexture(boxMaterial)
    //let texbox2 = Textures.mkMatTexture(boxMaterial2)
    //let texbox3 = Textures.mkMatTexture(boxMaterial3)
    //let texbox4 = Textures.mkMatTexture(boxMaterial4)
    //let texbox5 = Textures.mkMatTexture(boxMaterial5)
    //let texbox6 = Textures.mkMatTexture(boxMaterial6)
    //let box = new Box(low, high, texbox, texbox2, texbox3, texbox4, texbox5, texbox6)
    ////let box = new Box(low, high, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat)

    //let vlow = new Point(0., 0., 0.)
    //let vhigh = new Point(2., 2., 2.)
    //let vboxMaterial = new MatteMaterial(new Colour(0., 1., 1.), 1., Colour(0., 1., 1.), 1.)
    //let vboxMaterial2 = new MatteMaterial(new Colour(0., 0., 1.), 1., Colour(0., 0., 1.), 1.)
    //let vboxMaterial3 = new MatteMaterial(new Colour(1., 0., 1.), 1., Colour(1., 0., 1.), 1.)
    //let vboxMaterial4 = new MatteMaterial(new Colour(1., 1., 0.), 1., Colour(1., 1., 0.), 1.)
    //let vboxMaterial5 = new MatteMaterial(new Colour(0.5, 0.5, 1.), 1., Colour(0.5, 0.5, 1.), 1.)
    //let vboxMaterial6 = new MatteMaterial(new Colour(0.3, 0., 0.6), 1., Colour(0.3, 0., 0.6), 1.)
    ////let vbox = new Box(vlow, vhigh, vboxMaterial, vboxMaterial2, vboxMaterial3, vboxMaterial4, vboxMaterial5, vboxMaterial6)
    ////let box = new Box(low, high, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat, GlossyBoxMat)

    //let texPlane = Textures.mkMatTexture(matteRed)
    //let infinitePlane = InfinitePlane(texPlane)
    ////let csgShape3 = CSG(box, csgShape2, Intersection)

    //let bLeft = new Point(0., 0., 0.)
    //let tLeft = new Point(0., 1., 0.)
    //let bRight = new Point(1., 0., 0.)
    //let texRectangle = Textures.mkMatTexture(matteRed)
    //let rectangle = Rectangle(bLeft, tLeft, bRight, texRectangle) 

    //let discCenter = new Point(0., 0., 0.)
    //let discRadius = 1.
    //let texDisc = Textures.mkMatTexture(matteBlue)
    //let disc = Disc(discCenter, discRadius, texDisc)

    //let a = new Point(0., 0., 0.)
    //let b = new Point(0., 1., 0.)
    //let c = new Point(1., 0., 0.)
    //let triangleMaterial = new MatteMaterial(new Colour(0., 1., 1.), 1., new Colour(0., 1., 1.), 1.)
    //let texTri = Textures.mkMatTexture(triangleMaterial)
    //let triangle = Triangle(a, b, c, new MatteMaterial(new Colour(0., 1., 1.), 1., new Colour(0., 1., 1.), 1.))

    //let solidOrigin = new Point(0., 0., 0.)
    //let solidRadius = 0.5
    //let solidHeight = 2.
    //let texSolid = Textures.mkMatTexture(matteGreen)
    //let texSolid2 = Textures.mkMatTexture(matteBlue)
    //let texSolid3 = Textures.mkMatTexture(matteRed)
    //let solidCylinder = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    //let solidCylinder2 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    //let solidCylinder3 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    //let solidCylinder4 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    //let solidCylinder5 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    //let solidCylinder6 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    //let solidCylinder7 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    //let solidCylinder8 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    //let solidCylinder9 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)
    //let solidCylinder10 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolid, texSolid3, texSolid2)

    //let csgTestInsideEdges = CSG(sphere, box, Union)

    //let csgShape = CSG(sphere, box, Intersection)
    //let csgShape2 = CSG(csgShape, solidCylinder, Union)
    //let csgShape3 = CSG(box, csgShape2, Intersection)

    //let csgSub = CSG(box, sphere, Subtraction)


    //let move = Transformation.translate 0. -1. 0.
    //let transCylinder = Transform.transform cylinder move
    //let transCSG = Transform.transform csgShape3 move

    ////Fancy CSG
    //let texSolidYellow = Textures.mkMatTexture(matteYellow)
    //let texSolidGreen = Textures.mkMatTexture(matteGreen)
    //let texSolidBlue = Textures.mkMatTexture(matteBlue)
    //let solid1 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolidYellow, texSolidYellow, texSolidYellow)
    //let solid2 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolidYellow, texSolidYellow, texSolidYellow)
    //let solid3 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolidYellow, texSolidYellow, texSolidYellow)

    ////let boxForcsg = Box(Point(-0.5,-0.5,-0.5), Point(0.5,0.5,0.5), boxTex, boxTex, boxTex, boxTex, boxTex, boxTex)
    ////let sphereForcsg = SphereShape(Point(0.,0.,0.), 0.65, Textures.mkMatTexture(matteBlue))

    //let solid2 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolidGreen, texSolidGreen, texSolidGreen)
    //let solid3 = SolidCylinder(solidOrigin, solidRadius, solidHeight, texSolidBlue, texSolidBlue, texSolidBlue)

    //let pi = Math.PI

    //let rotateX = Transformation.rotateX (pi/2.)
    //let rotateZ = Transformation.rotateZ (pi/2.)

    //let transSolid2 = Transform.transform solid2 rotateX
    //let transSolid3 = Transform.transform solid3 rotateZ

    //let csgUnion1 = CSG(solid1, transSolid2, Union)
    //let csgUnion2 = CSG(csgUnion1, transSolid3, Union)
    ////let boxSubTest = new Box(lowTest, highTest, texbox, texbox2, texbox3, texbox4, texbox5, texbox6)
    ////let boxSubTest2 = new Box(lowTest, highTest, texbox, texbox2, texbox3, texbox4, texbox5, texbox6)

    //let boxTex = Textures.mkMatTexture(matteRed)
    //let boxForcsg = Box(Point(-0.5,-0.5,-0.5), Point(0.5,0.5,0.5), boxTex, boxTex, boxTex, boxTex, boxTex, boxTex)
    //let boxForcsg2 = Box(Point(-1.2,-1.2,-1.2), Point(1.2,1.2,1.2), boxTex, boxTex, boxTex, boxTex, boxTex, boxTex)
    //let sphereForcsg = SphereShape(Point(0.,0.,0.), 0.9, Textures.mkMatTexture(matteBlue))

    //let csgInter = CSG(boxForcsg, sphereForcsg, Intersection)
    ////let csgBoxUnion2 = CSG(csgBoxUnion, transBox2, Union)

    //let fancyCSG = CSG(csgInter, csgUnion2, Subtraction)

    ////Subtraction Test

    //let csgSubTest = CSG(boxForcsg, csgUnion1, Subtraction)
    
    //let lowTest = new Point(-1., -2., -1.)
    //let highTest = new Point(1., 2., 1.)
    //let boxSubTest = new Box(lowTest, highTest, texbox, texbox2, texbox3, texbox4, texbox5, texbox6)
    //let boxSubTest2 = new Box(lowTest, highTest, texbox, texbox2, texbox3, texbox4, texbox5, texbox6)

    //let transBox = Transform.transform boxSubTest2 rotateX
    //let transBox2 = Transform.transform boxSubTest2 rotateZ
    //////- CAMERA
    ////let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY, regular 1)
    ////let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 4.0, 3.0, regular 1, regular 1)
    //////                        new SampleGenerator(multiJittered, VIEW_SAMPLES, CAM_SETS),
    //////                        new SampleGenerator(multiJittered, LENS_SAMPLES, CAM_SETS))
    
    //////- LIGHTS
    ////let lightFront     = PointLight(Colour.White, 1.5, Point(2.,2.,7.))

    //let csgBoxUnion = CSG(boxSubTest, transBox, Union)
    //let csgBoxUnion2 = CSG(csgBoxUnion, transBox2, Union)

    //let sphereRadiusTest = 0.8
    //let sphereSubTest = new SphereShape(sphereOrigin, sphereRadiusTest, texSphere)

    //let csgSubTest = CSG(boxForcsg2, csgBoxUnion2, Subtraction)

    //let csgSubTestUltra = CSG(sphereSubTest, csgUnion2, Subtraction)


    ////optimization
    //let rect1 = Rectangle(bLeft, tLeft, bRight, texRectangle)
    //let rect2 = Rectangle(bLeft, tLeft, bRight, texRectangle)
    //let rect3 = Rectangle(bLeft, tLeft, bRight, texRectangle)
    //let rect4 = Rectangle(bLeft, tLeft, bRight, texRectangle)
    //let rect5 = Rectangle(bLeft, tLeft, bRight, texRectangle)
    //let rect6 = Rectangle(bLeft, tLeft, bRight, texRectangle)
    //let rect7 = Rectangle(bLeft, tLeft, bRight, texRectangle)
    //let rect8 = Rectangle(bLeft, tLeft, bRight, texRectangle)
    //let rect9 = Rectangle(bLeft, tLeft, bRight, texRectangle)
    //let rect10 = Rectangle(bLeft, tLeft, bRight, texRectangle)

    //let sphere1 = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    //let sphere2 = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    //let sphere3 = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    //let sphere4 = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    //let sphere5 = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    //let sphere6 = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    //let sphere7 = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    //let sphere8 = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    //let sphere9 = new SphereShape(sphereOrigin, sphereRadius, texSphere)
    //let sphere10 = new SphereShape(sphereOrigin, sphereRadius, texSphere)

    //let cylinder1 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)
    //let cylinder2 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)
    //let cylinder3 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)
    //let cylinder4 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)
    //let cylinder5 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)
    //let cylinder6 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)
    //let cylinder7 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)
    //let cylinder8 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)
    //let cylinder9 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)
    //let cylinder10 = new HollowCylinder(cylinderOrigin, radius, cylinderHeight, texCylinder)

    //let texRectangle1 = Textures.mkMatTexture(matteRed)
    //let texRectangle2 = Textures.mkMatTexture(matteBlue)
    //let rectangleTest1 = Rectangle(Point(-1., -1., -1.), Point(-1., 0., -1.), Point(0., -1., -1.), texRectangle1) 
    //let rectangleTest2 = Rectangle(Point(0., 0., 0.), Point(0., 1., 0.), Point(1., 0., 0.), texRectangle2)


    //let shapes : Shape List = [rectangleTest2]
    ////let shapes : Shape List = [rect1;rect2;rect3;rect4;rect5;rect6;rect7;rect8;rect9;rect10]
    ////let shapes : Shape List = [sphere1;sphere2;sphere3;sphere4;sphere5;sphere6;sphere7;sphere8;sphere9;sphere10]
    ////let shapes : Shape List = [cylinder1;cylinder2;cylinder3;cylinder4;cylinder5;cylinder6;cylinder7;cylinder8;cylinder9;cylinder10]
    ////let shapes : Shape List = [solidCylinder;solidCylinder2;solidCylinder3;solidCylinder4;solidCylinder5;solidCylinder6;solidCylinder7;
    //                            //solidCylinder8;solidCylinder9;solidCylinder10]
    ////let shapes : Shape List = [csgUnion2;csgUnion2;csgUnion2;csgUnion2;csgUnion2;csgUnion2;csgUnion2;csgUnion2;csgUnion2;csgUnion2;]
    ////let shapes : Shape List = [csgInter;csgInter;csgInter;csgInter;csgInter;csgInter;csgInter;csgInter;csgInter;csgInter;]
    ////let shapes : Shape List = [csgSubTestUltra;csgSubTestUltra;csgSubTestUltra;csgSubTestUltra;csgSubTestUltra;csgSubTestUltra;csgSubTestUltra;csgSubTestUltra;csgSubTestUltra;csgSubTestUltra;]



    ////- THIN LENS SAMPLE SETTINGS
    //let CAM_SETS = 129
    //let VIEW_SAMPLES = 8
    //let LENS_SAMPLES = 8

    ////- CAMERA
    //let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY, multiJittered 1 1)
    ////let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 4.0, 3.0,
    ////                        new SampleGenerator(multiJittered, VIEW_SAMPLES, CAM_SETS),
    ////                        new SampleGenerator(multiJittered, LENS_SAMPLES, CAM_SETS))
    
    ////- LIGHTS
    //let lightFront     = PointLight(Colour.White, 1.5, Point(2.,2.,7.))
    //let lightTop       = DirectionalLight(Colour.White, 1., Vector(1.,3.,0.))
    //let lightBack     = PointLight(Colour.White, 1.5, Point(-2.,-2.,-7.))
    //let lightRight     = PointLight(Colour.White, 1., Point(-30., 0., 0.))

    //let lightAmbient   = AmbientLight(Colour.White, 0.3)
    ////let lightSphere    = SphereAreaLight(emissive, sC, 100, 5)
    ////let lightDisc      = DiscAreaLight(emissive, disc, 100, 5)
    ////let lightRect      = RectangleAreaLight(emissive, rectangle, 100, 5)
    ////let plane          = InfinitePlane(matteWhite)
    ////- FINAL
    //let lights: Light list      = [lightFront;]
    ////let spheres: Shape list     = [sL;sC;sR;plane]
    //let scene                   = Scene(shapes, lights, lightAmbient, 100)

    //let render = new Render(scene, camera)

    //ignore (render.RenderToFile render.RenderParallel "TracedRays.bmp")

    //0

  // Scene
  let mkScene' s =
    let light = PointLight (Colour.White, 0.5, Point(4.0, 2.0, 4.0))
    let light2 = PointLight (Colour.White, 0.5, Point(-4.0, 2.0, 4.0))
    let ambientLight = AmbientLight(Colour.White, 0.1)
    let (lights:Light list) = [light;light2]
    Scene ([s], lights, ambientLight, 0)

  // Colours
  let aqua = Colour (Color.Aqua)
  let white = Colour (Color.White)
    
  // Helper functions
  let mkshape (bs:baseShape) t = bs.toShape t

  let planeZmat = MatteMaterial (Colour (Color.Blue), 1.0, Colour (Color.Blue), 1.0)
  let planeZ = mkshape (mkImplicit "z") (mkMatTexture planeZmat)
  let planeZcam = PinholeCamera (Point (0.0, 0.0, 4.0), Point (0.0, 0.0, 0.0), Vector (0.0, 1.0, 0.0), 2.0, 4.0, 4.0, 500, 500, regular 1)

  // Shapes, cams, and colours
  let sphere1mat = PhongMaterial (aqua, 0.2, aqua, 0.8, white, 0.7, 100)
  let sphere1 = mkshape (mkImplicit "x^2 + y^2 + z^2 - 1.0") (mkMatTexture sphere1mat)
  let sphere1 = SphereShape(Point.Zero, 1.0, mkMatTexture sphere1mat)
  let sphere1cam = PinholeCamera (Point(0.0, 0.0, 4.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 3.0, 1024, 768, multiJittered 4 87)

  let sphere2mat = MatteMaterial (Colour.Blue, 1.0, Colour.Blue, 1.0)
  let sphere2 = mkshape (mkImplicit "(x^2 + y^2 + z^2)_2 - 1.0") (mkMatTexture sphere2mat)
  let sphere2cam = PinholeCamera (Point(0.0, 0.0, 4.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 4.0, 500, 500, regular 1)
    
  let torus = mkshape (mkImplicit "(((x^2 + y^2)_2 - 1.5)^2 + z^2)_2 - 0.5") (mkMatTexture sphere2mat)
  let toruscam = PinholeCamera (Point(0.0, 0.0, 4.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 4.0, 500, 500, regular 1)
    
  let linktorus1mat = mkMatTexture (PhongReflectiveMaterial (Colour(Color.Orange), 0.2, Colour(Color.Orange), 0.8, Colour.White, 0.3, Colour.White, 1.0, 100))
  let linktorus2mat = mkMatTexture (PhongReflectiveMaterial (Colour(Color.Violet), 0.2, Colour(Color.Violet), 0.8, Colour.White, 0.3, Colour.White, 1.0, 100))
  let linktorus =
    let bs = mkImplicit ("(((x^2 + y^2)_2 - " + (string 2.0) + ")^2 + z^2)_2 - " + (string 0.3))
    let t1 = transform (bs.toShape linktorus1mat) (translate -1.5 0.0 0.0)
    let t2 = transform (bs.toShape linktorus2mat) (mergeTransformations [rotateX 90.0; translate 1.5 0.0 0.0])
    new CSG(t1, t2, CSGOperator.Grouping) :> shape
  let linktoruscam = PinholeCamera (Point (0.0, 0.0, 6.0), Point(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), 2.0, 4.0, 3.0, 1024, 768, multiJittered 4 83)

  let torus2 =
    let rs1 = "(1.5^2 + 0.5^2)"
    let rs2 = "(1.5^2 - 0.5^2)"
    let sx = "x^4 + 2x^2*y^2 + 2x^2*z^2 - 2*" + rs1 + "*x^2"
    let sy = "y^4 + 2y^2*z^2 + 2*" + rs2 + "*y^2"
    let sz = "z^4 - 2*" + rs1 + "*z^2"
    let sc = rs2 + "^2"
    let eqn = sx + " + " + sy + " + " + sz + " + " + sc 
    //let eqn = "x^4+2x^2*y^2+2x^2*z^2+(-5x)^2+y^4+2y^2*z^2+4y^2+z^4+(-5z^2)+4"
    mkshape (mkImplicit eqn) (mkMatTexture sphere2mat)
  let torus2cam = PinholeCamera (Point(0.0, 4.0, 0.0), Point(0.0, 0.0, 0.0), Vector(0.0, 0.0, 1.0), 2.0, 4.0, 4.0, 500, 500, regular 1)

  let testshapemat = MatteMaterial (Colour(Color.Gold), 1.0, Colour(Color.Gold), 1.0)
  let testshape = mkshape (mkImplicit "(x - 2)^2*(x+2)^2 + (y - 2)^2(y+2)^2 + (z - 2)^2(z+2)^2 + 3(x^2*y^2 + x^2z^2 + y^2z^2) + 6x y z - 10(x^2 + y^2 + z^2) + 22") (mkMatTexture testshapemat)
  let testshapecam = PinholeCamera (Point(6.0, 6.0, 8.0), Point(0.0, 0.0, 0.0), Vector(-1.0, -1.0, 0.0), 2.0, 4.0, 4.0, 500, 500, regular 1)
    
  let heartmat = MatteMaterial (Colour(Color.DarkRed), 1.0, Colour(Color.DarkRed), 1.0)
  let heart = mkshape (mkImplicit "(x^2 + (4.0/9.0)*(y+1)^2 + z^2 - 1)^3 - x^2 * z^3 - (9.0/80.0)*(y+1)^2*z^3") (mkMatTexture heartmat)
  let heartcam = PinholeCamera (Point(0.0, 3.0, 1.0), Point(0.0, 0.0, 0.0), Vector(0.0, 0.0, 1.0), 2.0, 4.0, 4.0, 500, 500, regular 1)

  let factorial x = 
    if x = 0 then 1 else
    let rec fac_aux a acc =
      if a >= x then
        a * acc
      else
        fac_aux (a + 1) (a * acc)
    fac_aux 1 x

  let comb a b = 
    let x = float (factorial a) in
    let y = float (factorial b) in
    let z = float (factorial (a - b)) in
      x / (y * z)

  let rec strSum n f : string =
    if n = 0 then
      f 0
    else
      f n + " + " + (strSum (n - 1) f)

  let chmutov degree =       
    let T x = strSum (degree / 2) (fun (k : int) -> (string (comb degree (2 * k))) + " * (" + x + "^2 + -1.0)^" + (string k) + " * " + x + "^" + (string (degree - (2 * k))))
    let is = mkImplicit (T "x" + " + " + T "y" + " + " + T "z")
    let s = mkshape is (mkMatTexture testshapemat)
    s
  let chmutovcam = PinholeCamera (Point (16.0, 16.0, 16.0), Point (0.0, -0.5, 0.0), Vector (-1.0, 1.0, -1.0), 16.0, 4.0, 4.0, 500, 500, regular 1)

  let render = Render(mkScene' planeZ, planeZcam)
  //let render = Render(mkScene' sphere1, sphere1cam)
  //let render = Render(mkScene' sphere2, sphere2cam)
  //let render = Render(mkScene' torus, toruscam)
  //let render = Render(mkScene' heart, heartcam)
  //let render = Render(mkScene' (chmutov 2), chmutovcam)
  //let render = Render(mkScene' (chmutov 3), chmutovcam)
  //let render = Render(mkScene' (chmutov 4), chmutovcam)
  //let render = Render(mkScene' (chmutov 5), chmutovcam)
  //let render = Render(mkScene' (chmutov 6), chmutovcam)
  //let render = Render(mkScene' torus2, torus2cam)
  //let render = Render(mkScene' testshape, testshapecam)
  //let render = Render(mkScene' linktorus, linktoruscam)

  render.RenderToScreen render.RenderParallel |> ignore
  Console.ReadKey () |> ignore
  
  0