open Tracer.Basics

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
    
    //- SPHERE SETTINGS
    let sphereOrigin = new Point(-4., -1.5, 0.)
    let sphereRadius = 2.
    let sphereMaterial = new MatteMaterial(new Colour(0., 1., 1.))
    let sphereMaterialSpecular = new BlinnPhongMaterial(1., new Colour(1., 1., 1.), 10., Colour.Red)
    let sphereMaterialBlinnPhong = new SpecularMaterial(0.1, new Colour(1., 1., 1.), 50., Colour.Blue)
    let blinnPhongSharpGreen = new BlinnPhongMaterial(0.1, new Colour(1., 1., 1.), 2., Colour.Green)
    let perfectReflection = new PerfectReflectionMaterial(1, sphereMaterial, new Colour(1., 1., 1.), 0.5)

    //- LIGHT SETTINGS
    let lightPosition = new Point(0.,-1000000.,0.)
    let lightIntensity = 1.
    let lightColour = new Colour(1.,1.,1.)

    //- INITIALIZE OBJECTS
    let camera       = new Camera(position, lookat, up, zoom, width, height, resX, resY)
    let light        = new PointLight(lightColour, lightIntensity, lightPosition)
    let light2       = new PointLight(lightColour, lightIntensity, new Point(20.,20.,10.))
    let ambientLight = new AmbientLight(lightColour, 0.05)
    let sphere       = new Sphere(sphereOrigin, sphereRadius, perfectReflection)
    let sphere2      = new Sphere(new Point(-4., 1.5, 0.), 1., sphereMaterialBlinnPhong) 
    let sphere3      = new Sphere(new Point(-4., 0., 3.), 1., sphereMaterialSpecular)
    //- FINAL 
    let lights: Light list      = [ambientLight;light2]
    let spheres: Sphere list    = [sphere;sphere2;sphere3]
    let scene                   = new Scene(spheres, camera, lights)

    printfn "Rendering ..."
    ignore scene.Render
    printfn "Finished!"

    0