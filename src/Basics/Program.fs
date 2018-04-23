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
    
    //- SPHERE SETTINGS
    let sphereOrigin = new Point(-2., -3., 0.)
    let sphereRadius = 2.
    let rawMatte = new MatteMaterial(Colour.White)
    let sphereMaterial = new MatteMaterial(Colour.Red)
    let sphereMaterialSpecular = new BlinnPhongMaterial(1., new Colour(1., 1., 1.), 10., Colour.Red)
    let sphereMaterialBlinnPhong = new BlinnPhongMaterial(0.15, new Colour(1., 1., 1.), 2., Colour.Blue)
    let blinnPhongSharpGreen = new BlinnPhongMaterial(0.1, new Colour(1., 1., 1.), 2., Colour.Green)
    let phongShades = new SpecularMaterial(0.15, new Colour(1.,1.,1.), 1.5, Colour.White)
    let perfectReflection = new PerfectReflectionMaterial(1, sphereMaterial, new Colour(1., 1., 1.), 1.)
    let perfectReflection2 = new PerfectReflectionMaterial(1, sphereMaterialBlinnPhong, new Colour(1., 1., 1.), 1.)
    let perfectReflection3 = new PerfectReflectionMaterial(1, sphereMaterialSpecular, new Colour(1., 1., 1.), 1.)
    let perfectReflection4 = new PerfectReflectionMaterial(1, blinnPhongSharpGreen, new Colour(1., 1., 1.), 1.)
    let glossyMaterial1 = new GlossyMaterial(2., Colour.White, rawMatte, 20, 30, 1, 2.)
    let niceShade = new MixedMaterial(rawMatte, phongShades, 0.5)
    let earthGroundMaterial = new TexturedMaterial(niceShade, "textures/Earth.png")
    let earthCloudsMaterial = new TexturedMaterial(niceShade, "textures/Earth-clouds.jpg")
    let earthMaterial = new AddMaterial(earthGroundMaterial, earthCloudsMaterial)
        

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
    let sphere       = new Sphere(sphereOrigin, sphereRadius, earthMaterial)
    let sphere4       = new Sphere(new Point(-2., 3., 0.), sphereRadius, rawMatte)
    let sphere2      = new Sphere(new Point(-2., 0.0, -1.), 1., perfectReflection) 
    let sphere3      = new Sphere(new Point(-2., 0.0, 1.1), 1., phongShades)
    let earth        = new Sphere(new Point(0.,0.,0.), 1.5, earthMaterial)
    let moon         = new Sphere(new Point(-1.,-1.0, 0.), 1.25, glossyMaterial1)

    //- FINAL 
    let lights: Light list      = [ambientLight; light]
    let spheres: Sphere list    = [earth]
    let scene                   = new Scene(spheres, camera, lights)

    printfn "Rendering ..."
    ignore scene.Render
    printfn "Finished!"

    0