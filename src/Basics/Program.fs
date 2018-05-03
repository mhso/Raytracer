open Tracer.Basics
open Tracer.Sampling.Sampling
open System.IO

[<EntryPoint>]
let main _ = 
    
    let position = Point(0.,0.,5.)
    let lookat = Point(0.,0.,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let resX = 1920
    let resY = 1080
    let width = 2.
    let height = (float(resY) / float(resX)) * width
    
    //- MATERIALS
    let x = MatteMaterial(Colour.Blue)

    //- SHAPES
    let sphere = SphereShape(Point(0.,0.,0.), 1., x)

    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 129
    let VIEW_SAMPLES = 8
    let LENS_SAMPLES = 8
    
    //- LIGHTS
    let directional = DirectionalLight(Colour.White, 1., Vector(1., 1., 1.))

    //- FINAL
    let camera                  = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY)
    let lights: Light list      = [directional]
    let spheres: Shape array    = [| sphere |]
    let scene                   = Scene(spheres, camera, lights)

    ignore scene.Render
    
    0