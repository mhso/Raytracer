open Tracer.Basics
open Tracer.Basics.Textures
open Tracer.Sampling.Sampling
open System.IO
open Tracer.BaseShape
open Tracer.Basics.Render
open Tracer.Basics
open Tracer.Basics

[<EntryPoint>]
let main _ = 
    // General settings
    let mkTextureFromFile (tr : float -> float -> float * float) (file : string) =
        let img = new System.Drawing.Bitmap(file)
        let width = img.Width - 1
        let height = img.Height - 1
        let widthf = float width
        let heightf = float height
        let texture x y =
          let (x', y') = tr x y
          let x'', y'' = int (widthf * x'), int (heightf * y')
          let c = lock img (fun () -> img.GetPixel(x'',y''))
          (MatteMaterial(Colour.White, 1., Colour(c), 1.)) :> Material
        mkTexture texture
    Acceleration.setAcceleration Acceleration.Acceleration.KDTree
    //let position = Point(-30.,140.,-200.) //Position for Armadillo
    //let position = Point(0.,1.,1.) //Position for Happy
    let position = Point(0.5,0.4,1.) //Position for bunny
    //let lookat = Point(0.,60.,0.) //Lookat for Armadillo
    //let lookat = Point(0.,0.1,0.) //LookAt for happy
    let lookat = Point(0.05,0.1,0.) //LookAt for bunny
    let up = Vector(0.,1.,0.)
    //let zoom = 1. //Normal zoom
    let zoom = 5. //Zoom for Happy
    let resX = 800
    let resY = 600
    let width = 2.
    let height = (float(resY) / float(resX)) * width
    let maxReflectionBounces = 3
    
    //- SAMPLE SETTINGS
    // Base sampling settings
    let BASE_SAMPLE_COUNT = 4
    let BASE_SET_COUNT = 127

    // Override these if needed
    let CAM_SETS = BASE_SET_COUNT
    let VIEW_SAMPLES = 2
    let LENS_SAMPLES = 8
    let MATERIAL_SAMPLES = BASE_SAMPLE_COUNT
    let MATERIAL_SETS = BASE_SET_COUNT

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
        then matteRed :> Material
        else glossyBlue :> Material
    let plane =  InfinitePlane(mkTexture(checker))


    let i = (TriangleMes.drawTriangles  @"..\..\..\..\resources\ply\bunny_textured.ply" true)
    let tex = mkTextureFromFile (fun x y -> (y,x)) @"..\..\..\..\resources\textures\bunny.png"
    let urn = i.toShape(tex)
    let t = Transformation.mergeTransformations
                [Transformation.rotateY (System.Math.PI/4.0);
                Transformation.scale 6.0 6.0 6.0]
    let bunnyShape = Transform.transform urn t

    //- CAMERA
    let camera        = PinholeCamera(Point(4.0,8.0,16.0), Point(0.0,0.5,0.0), Vector(0.0,1.0,0.0), 4.0, 5.66, 4.0, 1024, 768, regular 1)
    //let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 0.3, 8.0,
    //                        new SampleGenerator(multiJittered, VIEW_SAMPLES, CAM_SETS),
    //                        new SampleGenerator(multiJittered, LENS_SAMPLES, CAM_SETS))
    
    //- PLAIN LIGHTS
    let lightTop = DirectionalLight(Colour.White, 1., Vector(7., 7., 7.))

    //- AREA LIGHTS
    let sampler        = multiJittered 5 1
    let baseSphere = BaseSphere(Point.Zero, 1.)
    let baseRect = BaseRectangle(Point.Zero, Point(0., 1., 0.), Point(1., 0., 0.))
    let baseDisc = BaseDisc(Point.Zero, 1.)
    let lightSphere    = SphereAreaLight(emissive, baseSphere, sampler)
    let lightRect      = RectangleAreaLight(emissive, baseRect, sampler)
    let lightDisc      = DiscAreaLight(emissive, baseDisc, sampler)

    let directLight = DirectionalLight(Colour.White, 0.9, Vector(-1., 0., 0.))
    let mkPoint a b c = Point(a,b,c)
    let l1 = PointLight(Colour.White, 0.5, (mkPoint 6.0 2.0 6.0))
    let l2 = PointLight(Colour.Red, 0.5 ,(mkPoint -6.0 2.0 6.0))
    let l3 = PointLight(Colour.White, 1.0,(mkPoint -3.5 12.0 4.0))
    //- FINAL
    let lights: Light list      = [l1;l2;l3; lightTop]
    let shapes: Shape list      = [bunnyShape]

    let lightAmbient   = AmbientLight(Colour.Green, 0.0)
    let scene = Scene(shapes, lights, lightAmbient, maxReflectionBounces)


    let render = new Render(scene, camera)
    ignore (render.RenderToFile render.RenderParallel "image.bmp")

    0

