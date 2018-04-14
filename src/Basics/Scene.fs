namespace Tracer.Basics

open System.IO
open System.Drawing
open System

type Scene(sphere: Sphere, camera: Camera, lights: Light list) = 
    let sphere = sphere
    let camera = camera
    let lights = lights
    member this.Sphere = sphere
    member this.Camera = camera
    member this.Lights = lights
    member this.Cast (ray:Ray) (sphere:Sphere) = 

        let s = (ray.GetOrigin - sphere.Origin.ToVector).ToVector
        let rayDir = ray.GetDirection.Normalise
        let sv = s * rayDir
        let ss = s * s
        let D = sv*sv - ss + sphere.Radius * sphere.Radius
        if D < 0.0 then 
            new Colour(0.,0.,0.)
        else 
            let pointAtTime (ray:Ray) time = ray.GetOrigin + time * ray.GetDirection
            let normalAtTime t = (pointAtTime ray t - sphere.Origin)
            let (t1,t2) = (-sv + Math.Sqrt(D), -sv - Math.Sqrt(D))
            let hitPoint1 = pointAtTime ray t1
            let hitPoint2 = pointAtTime ray t2
            let closestHitPoint = 
                if hitPoint1.Distance(camera.Position).Magnitude < hitPoint2.Distance(camera.Position).Magnitude then
                    (hitPoint1, (normalAtTime t1))
                else
                    (hitPoint2, (normalAtTime t1))
            
            let mutable finalColour = new Colour(0.,0.,0.)
            for light in lights do
                finalColour <- finalColour + sphere.Material.Bounce closestHitPoint ray light
            finalColour

    member this.Render = 
            
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)
        let n = (camera.Position - camera.Lookat).Normalise
        let v = (n % camera.Up).Normalise
            
        // Field of view
        let hfov = Math.PI/3.5
        let vfov = hfov * float(camera.ResY)/float(camera.ResX)
        let pw = 2.0 * tan(float(hfov/2.0))/float(camera.ResX)
        let ph = 2.0 * tan(float(vfov/2.0))/float(camera.ResY)
        let backgroundColor = Color.Black
            
        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                let rayOrigin = camera.Position - n + float(x-camera.ResX/2)*pw*camera.Up + float(y-camera.ResY/2)*ph*v
                let rayDirection = (rayOrigin - camera.Position).Normalise
                let ray = new Ray(camera.Position,rayDirection)
                let colour = this.Cast ray sphere
                renderedImage.SetPixel(x, y, colour.ToColor)

        renderedImage.Save(camera.RenderFilepath)
        System.Diagnostics.Process.Start(camera.RenderFilepath)
        