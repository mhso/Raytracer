// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FParsec
open Tracer.Basics

[<EntryPoint>]
let main argv = 
    //let answer = PLYParser.parsePLY @"..\..\..\..\resources\ply\urn2.ply"
    //only used to draw the triangles
    let position = Point(7.,4.,4.)
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
    let emissive = EmissiveMaterial(Colour.White, 1.)

    //- SHAPES

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
    let p1 = Point(1.,0.,0.)
    let p2 = Point(0.,4.,0.)
    let p3 = Point(0.,0.,2.)
    let spheres: Shape array     = [|(TriangleMesh.drawTrianglesSpecificNumber 30  @"..\..\..\..\resources\ply\urn2.ply" false false)|]
    let scene                   = Scene(spheres, camera, lights)

    ignore scene.RenderParallel
    //System.Console.ReadKey() |> ignore
    0
