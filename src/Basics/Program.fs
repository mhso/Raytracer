open Tracer.Basics
open Tracer.Basics.Textures
open Tracer.Sampling.Sampling
open System.IO
open Tracer.BaseShape
open Tracer.Basics.Render
open Tracer.Basics.Transform
open Tracer.Basics
open System.Drawing

[<EntryPoint>]
let main _ = 
    // General settings
    Acceleration.setAcceleration Acceleration.Acceleration.KDTree
    let position = Tracer.Basics.Point(0.,2.9,8.0)
    let lookat = Tracer.Basics.Point(0.,0.1,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 4.0
    let resX = 1000
    let resY = 1000
    let width = 2.
    let height = 2.
    let maxReflectionBounces = 6
    
    //- SAMPLE SETTINGS
    // Base sampling settings
    let BASE_SAMPLE_COUNT = 4
    let BASE_SET_COUNT = 127

    // Override these if needed
    // Camera samples, Pinhole uses View Samples, thin lens uses View Samples and Lens Samples.
    let CAM_SETS = BASE_SET_COUNT
    let VIEW_SAMPLES = 1
    let LENS_SAMPLES = 2
    // Material sample values.
    let MATERIAL_SAMPLES = BASE_SAMPLE_COUNT
    let MATERIAL_SETS = BASE_SET_COUNT
    // Light sample values.
    let LIGHT_SAMPLES = 4
    let LIGHT_SETS = BASE_SET_COUNT


    let mkMatte c k = MatteMaterial(c,k,c,k) :> Material
    let white () = mkMatte (Colour(Color.White)) 1.
    let black () = mkMatte (Colour(Color.Black)) 1.
    let blue () = mkMatte (Colour(Color.Blue)) 1.
    let green () = mkMatte (Colour(Color.Green)) 1.
    let yellow () = mkMatte (Colour(Color.Yellow)) 1.
    let red () = mkMatte (Colour(Color.Red)) 1.
    let pink () = mkMatte (Colour(Color.Pink)) 1.
    let gray () = mkMatte (Colour(Color.Gray)) 1.
    let checker c x y =
      let abs' f = if f < 0.0 then 1.0 - (f*16.0) else f * 16.0
      if (int (abs' x) + int (abs' y)) % 2 = 0
      then (white ())
      else c
    let checkerTex c = mkTexture (checker (c ())) 

    //--------------- SCENE START -----------------



    let light = PointLight(Colour.White, 0.4, Tracer.Basics.Point(0.0, 4.0, -2.0))
    let ambientLight = AmbientLight(Colour.White, 0.6)
    let sphere1Raw = SphereShape(Point.Zero, 1.0, mkMatTexture (TransparentMaterial(Colour(1.,0.5,0.5), Colour.White, 1.5, 1.0)))
    let sphere2Raw = SphereShape(Point.Zero, 1.0, mkMatTexture (MatteReflectiveMaterial(Colour(1.,0.5,0.5), 0.1, Colour(1., 0.5, 0.5), 0.9, Colour.White, 1.)))
    let sphere1 = Transform.transform sphere1Raw (Transformation.translate 1. 1. 0.)
    let sphere2 = Transform.transform sphere2Raw (Transformation.translate -0.5 1.0 -4.0)
    let box = Box(Tracer.Basics.Point(-12., 0., -10.), Tracer.Basics.Point(12., 12., 10.), checkerTex blue, checkerTex red, checkerTex yellow, checkerTex green, checkerTex pink, checkerTex black)
    
    //---------------- SCENE END ------------------




    let shapes: Shape list = [sphere1; sphere2; box]
    let lights: Light list = [light]
    let scene = Scene(shapes, lights, ambientLight, maxReflectionBounces)
    let camera = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY, regular 1)
    let render = Render(scene, camera)
    ignore (render.RenderToFile render.RenderParallel "image.bmp")

    0
