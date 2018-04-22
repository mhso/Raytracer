open Tracer.Basics
open System.IO

[<EntryPoint>]
let main _ = 
    
    //- CAMERA SETTINGS
    let position = new Point(7.,0.,0.)
    let lookat = new Point(0.,0.,0.)
    let up = new Vector(0.,1.,0.)
    let zoom = 1.
    let width = 640.
    let height = 480.
    let resX = 640
    let resY = 480
    
    //- SPHERE SETTINGS
    let sphereOrigin = new Point(-2., -3., 0.)
    let sphereRadius = 2.
    let rawMatte = new MatteMaterial(Colour.White)
    let sphereMaterial = new MatteMaterial(Colour.Red)
    let sphereMaterialSpecular = new BlinnPhongMaterial(1., new Colour(1., 1., 1.), 10., Colour.Red)
    let sphereMaterialBlinnPhong = new BlinnPhongMaterial(0.15, new Colour(1., 1., 1.), 2., Colour.Blue)
    let blinnPhongSharpGreen = new BlinnPhongMaterial(0.1, new Colour(1., 1., 1.), 2., Colour.Green)
    let perfectReflection = new PerfectReflectionMaterial(1, sphereMaterial, new Colour(1., 1., 1.), 1.)
    let perfectReflection2 = new PerfectReflectionMaterial(1, sphereMaterialBlinnPhong, new Colour(1., 1., 1.), 1.)
    let perfectReflection3 = new PerfectReflectionMaterial(1, sphereMaterialSpecular, new Colour(1., 1., 1.), 1.)
    let perfectReflection4 = new PerfectReflectionMaterial(1, blinnPhongSharpGreen, new Colour(1., 1., 1.), 1.)
    let glossyMaterial1 = new GlossyMaterial(1., Colour.White, sphereMaterial, 100, 30, 1, 10.)
    let woodMaterial = new TexturedMaterial(rawMatte, "textures/earth.jpg")    
        

    //- LIGHT SETTINGS
    let lightPosition = new Point(8.,0.,0.)
    let lightIntensity = 1.
    let lightColour = new Colour(1.,1.,1.)

    //- INITIALIZE OBJECTS
    let camera       = new Camera(position, lookat, up, zoom, width, height, resX, resY)
    let light        = new PointLight(lightColour, lightIntensity, lightPosition)
    let light2       = new PointLight(lightColour, lightIntensity, new Point(0.,8.,0.))
    let light3       = new PointLight(lightColour, lightIntensity, new Point(8.,8.,8.))

    let ambientLight = new AmbientLight(lightColour, 0.05)
    let sphere       = new Sphere(sphereOrigin, sphereRadius, woodMaterial)
    let sphere4       = new Sphere(new Point(-2., 3., 0.), sphereRadius, rawMatte)
    let sphere2      = new Sphere(new Point(-2., 0.0, -1.), 1., glossyMaterial1) 
    let sphere3      = new Sphere(new Point(-2., 0.0, 1.1), 1., perfectReflection)

    //- FINAL 
    let lights: Light list      = [ambientLight;light]
    let spheres: Sphere list    = [sphere;sphere2;sphere3;sphere4]
    let scene                   = new Scene(spheres, camera, lights)

    printfn "Rendering ..."
    ignore scene.Render
    printfn "Finished!"

    0