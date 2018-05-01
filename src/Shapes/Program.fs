open Tracer.Shapes
open System
open Tracer.Basics

[<EntryPoint>]
let main _ = 
    
    //- CAMERA SETTINGS
    let position = new Point(5.,5.,5.)
    let lookat = new Point(0.,0.,0.)
    let up = new Vector(0.,1.,0.)
    let zoom = 1.
    let width = 640.
    let height = 480.
    let resX = 640
    let resY = 480
    
    // shape Settings
    let cylinderOrigin = new Point(0., 0., 0.)
    let radius = 0.5
    let height = 2.
    let Material = new MatteMaterial(new Colour(0., 0., 1.))
    let MaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let cylinder = new HollowCylinder(cylinderOrigin, radius, height, Material)

    let sphereOrigin = new Point(0., 0., 0.)
    let sphereRadius = 1.
    let sphereMaterial = new MatteMaterial(new Colour(1., 1., 0.))
    let sphereMaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let sphere = new SphereShape(sphereOrigin, sphereRadius, sphereMaterial)

    let low = new Point(0., 0., 0.)
    let high = new Point(1., 1., 1.)
    let boxMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let boxMaterial2 = new MatteMaterial(new Colour(0., 0., 1.))
    let boxMaterial3 = new MatteMaterial(new Colour(1., 0., 1.))
    let boxMaterial4 = new MatteMaterial(new Colour(1., 1., 0.))
    let boxMaterial5 = new MatteMaterial(new Colour(0.5, 0.5, 1.))
    let boxMaterial6 = new MatteMaterial(new Colour(0.3, 0., 0.6))
    let sphereMaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let box = new Box(low, high, boxMaterial, boxMaterial2, boxMaterial3, boxMaterial4, boxMaterial5, boxMaterial6)

    let planeMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let infinitePlane = InfinitePlane(planeMaterial)


    let bLeft = new Point(0., 0., 0.)
    let tLeft = new Point(0., 1., 0.)
    let bRight = new Point(1., 0., 0.)
    let rectangleMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let rectangle = Rectangle(bLeft, tLeft, bRight, rectangleMaterial) 


    let discCenter = new Point(0., 0., 0.)
    let discRadius = 1.
    let discMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let disc = Disc(discCenter, discRadius, discMaterial)

    let a = new Point(0., 0., 0.)
    let b = new Point(0., 1., 0.)
    let c = new Point(1., 0., 0.)
    let triangleMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let triangle = Triangle(a, b, c, triangleMaterial)

    let shapes : Shape list = [infinitePlane]


    //- LIGHT SETTINGS
    let lightPosition = new Point(0.,-1000000.,0.)
    let lightIntensity = 1.
    let lightColour = new Colour(1.,1.,1.)

    //let camera = new Camera(position, lookat, up, zoom, width, height, resX, resY)
    let light = new PointLight(lightColour, lightIntensity, lightPosition)
    let light2 = new PointLight(lightColour, lightIntensity, new Point(20.,0.,0.))
    let ambientLight = new AmbientLight(lightColour, 0.1)
    //let scene = new SceneShapes(shapes, camera, [light2])
    
    printfn "Rendering ..."
    //ignore scene.Render
    printfn "Finished!"

    0