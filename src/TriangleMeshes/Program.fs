// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FParsec
open Tracer.Basics
open Tracer.Sampling
open Tracer.Sampling

[<EntryPoint>]
let main argv = 
    //let answer = PLYParser.parsePLY @"..\..\..\..\resources\ply\urn2.ply"
    //only used to draw the triangles
    Acceleration.setAcceleration (Acceleration.Acceleration.KDTree)
    let position = Point(0.,0.,0.)
    let lookat = Point(3.,3.,3.)
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
    let emissive = EmissiveMaterial(Colour.White, 1.)

    //- SHAPES
    let shape = (TriangleMes.drawTriangles  @"..\..\..\..\resources\ply\porsche.ply" false true matteWhite)
    //let shape = Transform.transform ico (Transformation.scale 50. 50. 50.)

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 29
    let VIEW_SAMPLES = 8
    let DISC_SAMPLES = 8

    //- CAMERA
    let camera         = PinholeCamera(Point (4.0, 8.0, 20.0), Point(0.,0.,0.), Vector(0.,1.,0.), 4., 2.5,2.5,1000,1000, Sampling.multiJittered 2 2)
    //- LIGHTS
    let lightFront     = PointLight(Colour.White, 1.5, Point(8.,-4.,0.))
    let lightTop       = DirectionalLight(Colour.White, 1., Vector(0.,-1.,0.))
    let lightAmbient   = AmbientLight(Colour.Green, 0.1)
    let l1 = PointLight(Colour.White, 0.5, Point(6.0, 2.0, 6.0))
    let l2 = PointLight(Colour.Red, 0.5, Point(-6.0, 2.0, 6.0))
    let l3 = PointLight(Colour.White, 0.7, Point(-3.5, 12.0, 4.0))

    //- FINAL
    let lights: Light list      = [lightAmbient; l1;l2;l3; lightTop]
    let p1 = Point(1.,0.,0.)
    let p2 = Point(0.,4.,0.)
    let p3 = Point(0.,0.,2.)



    let spheres: Shape array     = [|shape|]
    let scene                   = Scene(spheres, camera, lights, lightAmbient, 2)


    let acceleration = Acceleration.createAcceleration spheres
    ignore (scene.RenderParallel acceleration)
    //System.Console.ReadKey() |> ignore
    0
