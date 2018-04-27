open Tracer.Basics
open System.IO

[<EntryPoint>]
let main _ = 
    
    //- CAMERA SETTINGS
    let position = new Point(7.,0.,0.)
    let lookat = new Point(0.,0.,0.)
    let up = new Vector(0.,1.,0.)
    let zoom = 1.
    let width = 1920.
    let height = 1080.
    let resX = 1920
    let resY = 1080
    
    //- MATERIALS
    let matteRed = new MatteMaterial(Colour.Red)
    let matteGreen = new MatteMaterial(Colour.Green)
    let matteYellow = new MatteMaterial(Colour(1.,1.,0.))
    let matteWhite = new MatteMaterial(Colour.White)
    let phongShades = new SpecularMaterial(0.15, new Colour(1.,1.,1.), 1.5, Colour.White)
    let perfectWhite = new PerfectReflectionMaterial(5, matteWhite, Colour.White, 1.)
    let perfectGreen = new PerfectReflectionMaterial(5, matteGreen, Colour.White, 1.)
    let perfectRed = new PerfectReflectionMaterial(5, matteRed, Colour.White, 1.)
    let glossyWhite = new GlossyMaterial(5., Colour.White, matteWhite, 10, 1, 1, 100.)

    //- LIGHT SETTINGS
    let lightPosition = new Point(8.,-4.,0.)
    let lightIntensity = 1.5
    let lightColour = new Colour(1.,1.,1.)

    //- INITIALIZE OBJECTS
    let camera       = new Camera(position, lookat, up, zoom, width, height, resX, resY)
    let light        = new PointLight(lightColour, lightIntensity, lightPosition)
    let light2       = new PointLight(lightColour, lightIntensity, new Point(0.,8.,0.))
    let light3       = new PointLight(lightColour, lightIntensity, new Point(8.,8.,8.))
    let ambientLight = new AmbientLight(lightColour, 0.05)

    let sphereRed        = new SphereShape(Point(0.,-0.5,0.), 0.25, perfectRed)
    let sphereYellow     = new SphereShape(Point(0.,0.,0.), 0.25, glossyWhite)
    let sphereGreen      = new SphereShape(Point(0.,0.5,0.), 0.25, perfectGreen)

    //- FINAL 
    let lights: Light list      = [light;ambientLight]
    let spheres: Shape list     = [sphereRed; sphereYellow; sphereGreen]
    let scene                   = new Scene(spheres, camera, lights)

    printfn "Rendering ..."
    ignore scene.Render
    printfn "Finished!"

    0