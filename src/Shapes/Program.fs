open Tracer.Shapes
open System
open Tracer.Basics

[<EntryPoint>]
let main _ = 
    
    //- CAMERA SETTINGS
    let position = new Point(10.,0.,0.)
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
    let sphereMaterialBlinnPhong = new BlinnPhongMaterial(0.1, new Colour(1., 1., 1.), 20., new Colour(0., 0., 1.))
    let cylinder = new HollowCylinder(cylinderOrigin, radius, height, Material)

    let sphereOrigin = new Point(0., 0., 0.)
    let sphereRadius = 1.
    let sphereMaterial = new MatteMaterial(new Colour(1., 1., 0.))
    let sphereMaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let sphereMaterialBlinnPhong = new BlinnPhongMaterial(0.1, new Colour(1., 1., 1.), 20., new Colour(0., 0., 1.))
    let sphere = new SphereShape(sphereOrigin, sphereRadius, sphereMaterial)

    let low = new Point(0., 0., 0.)
    let high = new Point(1., 1., 1.)
    let boxMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let boxMaterial2 = new MatteMaterial(new Colour(0., 0., 1.))
    let sphereMaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let sphereMaterialBlinnPhong = new BlinnPhongMaterial(0.1, new Colour(1., 1., 1.), 20., new Colour(0., 0., 1.))
    let box = new Box(low, high, boxMaterial2, boxMaterial2, boxMaterial2, boxMaterial2, boxMaterial2, boxMaterial)

    let planeMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let infinitePlane = InfinitePlane(planeMaterial)


    let bLeft = new Point(0., 0., 0.)
    let tLeft = new Point(0., 1., 0.)
    let bRight = new Point(1., 0., 0.)
    let rectangleMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let rectangle = Rectangle(bLeft, tLeft, bRight, rectangleMaterial) 

    let shapes : Shape list = [rectangle]

    (*
    //- SPHERE SETTINGS
    let sphereOrigin = new Point(0., 0., 0.)
    let sphereRadius = 1.
    let sphereMaterial = new MatteMaterial(new Colour(0., 0., 1.))
    let sphereMaterialSpecular = new SpecularMaterial(1., new Colour(1., 1., 1.), 10., new Colour(0., 0., 1.))
    let sphereMaterialBlinnPhong = new BlinnPhongMaterial(0.1, new Colour(1., 1., 1.), 20., new Colour(0., 0., 1.))
    *)


    //- LIGHT SETTINGS
    let lightPosition = new Point(0.,-1000000.,0.)
    let lightIntensity = 1.
    let lightColour = new Colour(1.,1.,1.)

    let camera = new Camera(position, lookat, up, zoom, width, height, resX, resY)
    let shape = new HollowCylinder(cylinderOrigin, sphereRadius, height, sphereMaterialBlinnPhong)
    let light = new PointLight(lightColour, lightIntensity, lightPosition)
    let light2 = new PointLight(lightColour, lightIntensity, new Point(20.,0.,0.))
    let ambientLight = new AmbientLight(lightColour, 0.1)
    let scene = new SceneShapes(shapes, camera, [light2])
    
    printfn "Rendering ..."
    ignore scene.Render
    printfn "Finished!"

    0