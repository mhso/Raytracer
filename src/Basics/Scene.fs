namespace Tracer.Basics

open System.IO
open System.Drawing
open System

type Scene(spheres: Sphere list, camera: Camera, lights: Light list) = 
    let spheres = spheres
    let camera = camera
    let lights = lights
    let backgroundColour = new Colour(1.,1.,1.)
    member this.Spheres = spheres
    member this.Camera = camera
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour
    member this.Cast (ray:Ray) (spheres:Sphere list) = 

        let hitPointsThatHit = 
            [for s in spheres do yield (s, s.GetHitPoint ray)]
            |> List.filter (fun (_,hp) -> hp.DidHit)
        
        if hitPointsThatHit.IsEmpty then
            new Colour(1.,1.,1.)
        else
            // Get the closest hitpoint
            let closestHitPoint = hitPointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)
            let (sphere, hitPoint) = closestHitPoint
            let normal = sphere.NormalAtPoint hitPoint.Point

            let recursiveBounce (p:PerfectReflectionMaterial) (ray: Ray) (light:Light) = 
                let (finalSphere:Sphere, finalHitPoint:HitPoint) = 
                    this.CastRecursively (ray.PerfectReflect camera hitPoint.Point normal) spheres p.Bounces
                                        
                if(finalHitPoint.DidHit) then
                    let baseColor = p.PreBounce (hitPoint.Point, normal) ray light
                    let reflection = finalSphere.Material.PreBounce (finalHitPoint.Point, (finalSphere.NormalAtPoint finalHitPoint.Point)) ray light
                    baseColor * p.SpecularCoefficient + reflection * (1. - p.SpecularCoefficient)
                else
                    let baseColor = p.PreBounce (hitPoint.Point, normal) ray light
                    let reflection = backgroundColour
                    baseColor * p.SpecularCoefficient + reflection * (1. - p.SpecularCoefficient)

            // Sum the light colors for that hitpoint
            lights 
                |> List.fold (fun accColour light -> 
                    let colour = 
                        match sphere.Material with
                            | :? PerfectReflectionMaterial as p ->
                                recursiveBounce p ray light
                            | :? MixedMaterial as p ->
                                if p.MaterialA :? PerfectReflectionMaterial then
                                    let aCol = recursiveBounce (p.MaterialA :?> PerfectReflectionMaterial) ray light
                                    let bCol = p.MaterialB.PreBounce (hitPoint.Point, normal) ray light
                                    aCol.Scale(1.-p.Factor) + bCol.Scale(p.Factor)
                                elif p.MaterialB :? PerfectReflectionMaterial then
                                    let bCol = recursiveBounce (p.MaterialB :?> PerfectReflectionMaterial) ray light
                                    let aCol = p.MaterialA.PreBounce (hitPoint.Point, normal) ray light
                                    aCol.Scale(1.-p.Factor) + bCol.Scale(p.Factor)
                                else
                                    p.PreBounce (hitPoint.Point, normal) ray light
                                    
                            | _ -> 
                                sphere.Material.PreBounce (hitPoint.Point, normal) ray light
                    accColour + colour) (new Colour(0.,0.,0.))

    member this.CastRecursively (ray:Ray) (spheres:Sphere list) (bounces:int) = 
        let hitPointsThatHit = 
            [for s in spheres do yield (s, s.GetHitPoint ray)]
            |> List.filter (fun (_,hp) -> hp.DidHit)

        if hitPointsThatHit.IsEmpty then
            (Sphere.None, new HitPoint(ray))
        else

            let closestHitPoint = hitPointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)
            let (sphere, hitPoint) = closestHitPoint
            let normal = sphere.NormalAtPoint hitPoint.Point

            match sphere.Material with
            | :? PerfectReflectionMaterial as p -> 
                if bounces > 0 then
                    this.CastRecursively (ray.PerfectReflect camera hitPoint.Point normal) spheres (bounces-1)
                else 
                    (sphere, hitPoint)
            | _ -> (sphere, hitPoint)
            
                    
    member this.Render = 
            
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)
        let n = (camera.Position - camera.Lookat).Normalise
        let v = (n % camera.Up).Normalise
            
        // Field of view
        let hfov = Math.PI/3.5
        let vfov = hfov * float(camera.ResY)/float(camera.ResX)
        let pw = 2.0 * tan(float(hfov/2.0))/float(camera.ResX)
        let ph = 2.0 * tan(float(vfov/2.0))/float(camera.ResY)
            
        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                let rayOrigin = camera.Position - n + float(x-camera.ResX/2)*pw*camera.Up + float(y-camera.ResY/2)*ph*v
                let rayDirection = (rayOrigin - camera.Position).Normalise
                let ray = new Ray(camera.Position,rayDirection)
                let colour = this.Cast ray spheres
                renderedImage.SetPixel(x, y, colour.ToColor)

        renderedImage.Save(camera.RenderFilepath)
        System.Diagnostics.Process.Start(camera.RenderFilepath)
        