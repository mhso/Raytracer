open Tracer.Basics
open System.IO
open Transformation
open System

[<EntryPoint>]
let main _ = 
    
    //- CAMERA SETTINGS
    let position = new Point(18.,5.,5.)
    let lookat = new Point(0.,0.,0.)
    let up = new Vector(0.,1.,0.)
    let zoom = 1.
    let width = 1920.
    let height = 1080.
    let resX = 1920
    let resY = 1080
    
    //- SPHERE SETTINGS
    let sphereOrigin = new Point(-5., 0., 0.)
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
    let earthMaterial = new TexturedMaterial(niceShade,  @"..\..\..\..\resources\textures\earth.jpg")
    let perfectEarth = new CurryMaterial((fun a b -> a + b * 0.2),earthMaterial, perfectReflection)
    let purePerfect = PerfectReflectionMaterial(1, rawMatte, Colour.White, 1.)    

    //- LIGHT SETTINGS
    let lightPosition = new Point(8.,-4.,0.)
    let lightIntensity = 1.5
    let lightColour = new Colour(1.,1.,1.)

    //- INITIALIZE OBJECTS
    let camera       = new Camera(position, lookat, up, zoom, width, height, resX, resY)
    let light        = new PointLight(lightColour, lightIntensity, lightPosition)
    let light2       = new PointLight(lightColour, lightIntensity, new Point(0.,8.,0.))
    let light3       = new PointLight(lightColour, lightIntensity, new Point(8.,8.,8.))

    // Working
    let ambientLight = new AmbientLight(lightColour, 0.05)
    let sphere       = new SphereShape(sphereOrigin, sphereRadius, earthMaterial)
    let sphere4       = new SphereShape(new Point(-2., 3., 0.), sphereRadius, rawMatte)
    let sphere2      = new SphereShape(new Point(-2., 0.0, -1.), 1., perfectReflection) 
    let sphere3      = new SphereShape(new Point(-2., 0.0, 1.1), 1., phongShades)
    let earth        = new SphereShape(new Point(-3.,1.5,0.), 1.5, perfectEarth)
    let moon         = new SphereShape(new Point(-1.,-1.0, 0.), 1.25, glossyMaterial1)
    let transformedShape = transform sphere (sheare (1.,0.,0.,0.,0.,0.))
    let box = new Box(Point(-2.,-2.,-2.),Point(-1.,-1.,-1.),purePerfect,purePerfect,purePerfect,purePerfect,purePerfect,purePerfect)
    let multipleBoxTransformation = mergeTransformations [(translate 1. 0. 0.);(rotateY 1.2);(rotateX 0.8);(rotateZ 0.4);(sheare (1.,0.,0.,0.,0.,0.));(scale 1. -1. 1.)]
    let transformedBox = transform box multipleBoxTransformation

    
    let infinitePlane = new InfinitePlane(sphereMaterial)
    let cylinder = new HollowCylinder(Point(-2.,2.,-2.),1.,10.,sphereMaterial)

    //- FINAL 
    let lights: Light list      = [light;ambientLight]
    let spheres: Shape list    = [transformedShape; box; transformedBox]
    let scene                   = new Scene(spheres, camera, lights)

    printfn "Rendering ..."
    ignore scene.Render
    printfn "Finished!"

    0