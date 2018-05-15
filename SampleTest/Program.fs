open Tracer.Basics
open Tracer.Basics.Textures
open Tracer.Basics.Sampling
open System.IO
open Tracer.BaseShape
open Tracer.Basics.Render
open Tracer.Basics.Transform
open System.Diagnostics

(*
let simpleTraverse =
    let sampler = multiJittered 32 127
    let mutable res = 0.0
    let timer = Stopwatch.StartNew()
    for i in 0..1920 do
        for j in 0..1080 do
            let set = sampler.NextSet()
            for (x, y) in set do
                res <- res + x + y
    timer.Stop()
    printfn "New: %d ms" timer.ElapsedMilliseconds
    printfn "%f" res
let oldTraverse =
    let sampler = multiJittered 32 127
    let mutable res = 0.0
    let timer = Stopwatch.StartNew()
    for i in 0..1920 do
        for j in 0..1080 do
            for k in 0..sampler.SampleCount-1 do
                let (x, y) = sampler.Next()
                res <- res + x + y
    timer.Stop()
    printfn "Old: %d ms" timer.ElapsedMilliseconds
    printfn "%f" res
    *)

[<EntryPoint>]
let main _ = 
    // General settings
    Acceleration.setAcceleration Acceleration.Acceleration.KDTree
    let position = Point(5.,4.,5.)
    let lookat = Point(0.,2.,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let resX = 1000
    let resY = 700
    let width = 2.
    let height = (float(resY) / float(resX)) * width
    let maxReflectionBounces = 3
    
    //- SAMPLE SETTINGS
    // Base sampling settings
    let BASE_SAMPLE_COUNT = 4
    let BASE_SET_COUNT = 127

    // Override these if needed
    // Camera samples, Pinhole uses View Samples, thin lens uses View Samples and Lens Samples.
    let CAM_SETS = BASE_SET_COUNT
    let VIEW_SAMPLES = BASE_SAMPLE_COUNT
    let LENS_SAMPLES = BASE_SAMPLE_COUNT
    // Material sample values.
    let MATERIAL_SAMPLES = BASE_SAMPLE_COUNT
    let MATERIAL_SETS = BASE_SET_COUNT
    // Light sample values.
    let LIGHT_SAMPLES = BASE_SAMPLE_COUNT
    let LIGHT_SETS = BASE_SET_COUNT

    //- MATERIALS
    // Matte
    let matteRed = MatteMaterial(Colour.White, 1., Colour.Red, 1.)
    let matteGreen = MatteMaterial(Colour.White, 1., Colour.Green, 1.)
    let matteYellow = MatteMaterial(Colour.White, 1., Colour.Yellow, 1.)
    let matteWhite = MatteMaterial(Colour.White, 1., Colour.White, 1.)
    let matteBlue = MatteMaterial(Colour.White, 1., Colour.White, 1.)
    // Perfect
    let perfectRed = MatteReflectiveMaterial(Colour.White, 1., Colour.Red, 1., Colour.White, 1.)
    let perfectGreen = MatteReflectiveMaterial(Colour.White, 1., Colour.Red, 1., Colour.White, 1.)
    let perfectYellow = MatteReflectiveMaterial(Colour.White, 1., Colour.Yellow, 1., Colour.White, 1.)
    let perfectBlue = MatteReflectiveMaterial(Colour.White, 1., Colour.Blue, 1., Colour.White, 1.)
    let perfectWhite = MatteReflectiveMaterial(Colour.White, 1., Colour.White, 1., Colour.White, 1.)
    // Glossy
    let glossyBlue = PhongGlossyReflectiveMaterial(Colour.White, 1., Colour.Blue, 1., Colour.White, 1., Colour.White, 1., 2, 3,
                        multiJittered MATERIAL_SAMPLES MATERIAL_SETS)

    let emissive = EmissiveMaterial(Colour.White, 10000.)
    

    //- SHAPES
    let sphereRed        = SphereShape(Point(-5.,0.,2.), 0.5, mkMatTexture matteRed)
    let spherePerfectYellow     = SphereShape(Point(-2.,0.,0.), 0.5, mkMatTexture matteYellow)
    let sphereGreen      = SphereShape(Point(1.,0.,-2.), 0.5, mkMatTexture matteGreen)
    
    let matRedTex = mkMatTexture matteRed
    let matGreenTex = mkMatTexture matteGreen
    let matBlueTex = mkMatTexture matteBlue
    let matYellowTex = mkMatTexture matteYellow

    let sL = SphereShape(Point(-1., 0., -1.), 1., mkMatTexture matteRed)
    let sC = SphereShape(Point(0., 0., 0.), 1., mkMatTexture matteYellow)
    let sR = SphereShape(Point(1., 0., 1.), 1., mkMatTexture matteGreen)

    let transp = mkMatTexture (TransparentMaterial(Colour.Blue, Colour.Black, 0.8, 1.2))

    // Rectangles for testing Thin Lens.
    let thinBoxL = Box(Point(-5.5, 0., -6.), Point(-3.5, 3., -5.), matBlueTex, matRedTex, matBlueTex, matBlueTex, matBlueTex, matBlueTex)
    let thinBoxC = Box(Point(-1.5, 0., -4.), Point(0.5, 3., -3.), transp, transp, transp, transp, transp, transp)
    let thinBoxR = Box(Point(2., 0., -1.), Point(4., 3., 0.), matGreenTex, matGreenTex, matBlueTex, matBlueTex, matBlueTex, matBlueTex)

    let sTop = SphereShape(Point(0., 0., 10.), 5., Textures.mkMatTexture matteWhite)
    let discC = Disc(Point(0., 0., 0.), 4., Textures.mkMatTexture emissive)
    let rectC = Rectangle(Point(-4., -4., 0.), Point(-4., 4., 0.), Point(4., -4., 0.), Textures.mkMatTexture emissive)
    let checker x y =
        let abs' f = if f < 0.0 then 1.0 - (f*2.0) else f * 2.0
        if (int (abs' x) + int (abs' y)) % 2 = 0
        then perfectWhite :> Material
        else (new MatteMaterial(Colour.White, 1., Colour.Black, 1.)) :> Material
    let plane =  InfinitePlane(mkTexture(checker))

    //- CAMERA
    //let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY, multiJittered VIEW_SAMPLES CAM_SETS)
    let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 0.3, 8.0,
                            multiJittered VIEW_SAMPLES CAM_SETS,
                            multiJittered LENS_SAMPLES CAM_SETS)
    
    //- PLAIN LIGHTS
    let lightTop = DirectionalLight(Colour.White, 1., Vector(7., 7., 7.))

    //- AREA LIGHTS
    let baseSphere = BaseSphere(Point.Zero, 1.)
    let baseRect = BaseRectangle(Point.Zero, Point(0., 1., 0.), Point(1., 0., 0.))
    let baseDisc = BaseDisc(Point.Zero, 1.)
    let lightSphere    = SphereAreaLight(emissive, baseSphere, multiJittered LIGHT_SAMPLES LIGHT_SETS)
    let lightRect      = RectangleAreaLight(emissive, baseRect, multiJittered LIGHT_SAMPLES LIGHT_SETS)
    let lightDisc      = DiscAreaLight(emissive, baseDisc, multiJittered LIGHT_SAMPLES LIGHT_SETS)

    //- FINAL
    let lights: Light list      = [lightTop]
    let shapes: Shape list      = [thinBoxL; thinBoxC; thinBoxR; plane;]

    let lightAmbient   = AmbientLight(Colour.White, 0.05)
    let scene = Scene(shapes, lights, lightAmbient, maxReflectionBounces)

    let render = new Render(scene, camera)
    ignore (render.RenderToScreen render.RenderParallel)

    0