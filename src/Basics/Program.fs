open Tracer.Basics
open Tracer.Basics.Textures
open Tracer.Sampling.Sampling
open System.IO

[<EntryPoint>]
let main _ = 
    Acceleration.setAcceleration Acceleration.Acceleration.KDTree
    let position = Point(0.,2.,5.)
    let lookat = Point(0.,2.,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let resX = 640
    let resY = 480
    let width = 2.
    let height = (float(resY) / float(resX)) * width
    let maxReflectionBounces = 3
    

    //- MATERIALS
    let matteRed = MatteMaterial(Colour.Red)
    let matteGreen = MatteMaterial(Colour.Green)
    let matteYellow = MatteMaterial(Colour(1.,1.,0.))
    let matteWhite = MatteMaterial(Colour.White)
    let matteBlue = MatteMaterial(Colour.Blue)
    let phongShades = SpecularMaterial(0.15, Colour(1.,1.,1.), 1.5, Colour.White)
    let perfectWhite = PerfectReflectionMaterial(matteWhite, Colour.White, 1.)
    let perfectRed = PerfectReflectionMaterial(matteRed, Colour.White, 1.)
    let perfectGreen = PerfectReflectionMaterial(matteGreen, Colour.White, 1.)
    let perfectYellow = PerfectReflectionMaterial(matteYellow, Colour.White, 1.)
    let perfectBlue = PerfectReflectionMaterial(matteBlue, Colour.Red, 1.)
    let glossyWhite = GlossyMaterial(5., Colour.White, matteWhite, 10, 127, 100.)
    let emissive = EmissiveMaterial(Colour.White, 10000.)
    

    //- SHAPES
    let sphereRed        = SphereShape(Point(-5.,0.,2.), 0.5, mkMatTexture matteRed)
    let spherePerfectYellow     = SphereShape(Point(-2.,0.,0.), 0.5, mkMatTexture matteYellow)
    let sphereGreen      = SphereShape(Point(1.,0.,-2.), 0.5, mkMatTexture matteGreen)
    
    let matRedTex = mkMatTexture matteRed
    let matGreenTex = mkMatTexture matteGreen
    let matBlueTex = mkMatTexture matteBlue
    let matYellowTex = mkMatTexture matteYellow

    let perfRedTex = mkMatTexture perfectRed
    let perfYellowTex = mkMatTexture perfectYellow
    let perfGreenTex = mkMatTexture perfectGreen
    let perfBlueTex = mkMatTexture perfectBlue

    let sL = SphereShape(Point(-1., 0., -1.), 1., mkMatTexture matteRed)
    let sC = SphereShape(Point(0., 0., 0.), 1., mkMatTexture matteYellow)
    let sR = SphereShape(Point(1., 0., 1.), 1., mkMatTexture matteGreen)

    // Rectangles for testing Thin Lens.
    let thinBoxL = Box(Point(-5.5, 0., -6.), Point(-3.5, 3., -5.), matRedTex, matRedTex, matBlueTex, matBlueTex, matBlueTex, matBlueTex)
    let thinBoxC = Box(Point(-1.5, 0., -4.), Point(0.5, 3., -3.), matYellowTex, matYellowTex, matBlueTex, matBlueTex, matBlueTex, matBlueTex)
    let thinBoxR = Box(Point(2., 0., -1.), Point(4., 3., 0.), matGreenTex, matGreenTex, matBlueTex, matBlueTex, matBlueTex, matBlueTex)


    let sTop = SphereShape(Point(0., 0., 10.), 5., Textures.mkMatTexture matteWhite)
    let discC = Disc(Point(0., 0., 0.), 4., Textures.mkMatTexture emissive)
    let rectC = Rectangle(Point(-4., -4., 0.), Point(-4., 4., 0.), Point(4., -4., 0.), Textures.mkMatTexture emissive)
    let checker x y =
        let abs' f = if f < 0.0 then 1.0 - (f*2.0) else f * 2.0
        if (int (abs' x) + int (abs' y)) % 2 = 0
        then new MatteMaterial(new Colour(0., 0., 0.)) :> Material
        else perfectYellow :> Material
    let plane =  InfinitePlane(mkTexture(checker))

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 129
    let VIEW_SAMPLES = 8
    let LENS_SAMPLES = 8

    //- CAMERA
    let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY, multiJittered 10 1)
    //let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 0.3, 8.0,
    //                        new SampleGenerator(multiJittered, VIEW_SAMPLES, CAM_SETS),
    //                        new SampleGenerator(multiJittered, LENS_SAMPLES, CAM_SETS))

    //- LIGHTS
    let lightFront     = PointLight(Colour.White, 0.5, Point(7., 7., 7.))

    let lightAmbient   = AmbientLight(Colour.White, 0.1)
    let lightSphere    = SphereAreaLight(emissive, sC, 100, 5)
    let lightDisc      = DiscAreaLight(emissive, discC, 100, 5)
    let lightRect      = RectangleAreaLight(emissive, rectC, 100, 5)

    //- FINAL
    let lights: Light list      = [lightAmbient; lightFront]
    let shapes: Shape[]        = [|thinBoxC;thinBoxL;thinBoxR;plane|]

    let scene = Scene(shapes, camera, lights, lightAmbient, maxReflectionBounces)

    let acceleration = Acceleration.createAcceleration shapes
    ignore (scene.Render acceleration)
    

    0