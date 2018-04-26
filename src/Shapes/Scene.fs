namespace Tracer.Shapes

open Tracer.Basics
open System
open System.Drawing
open System.Diagnostics


type SceneShapes(shapes: Shape list, camera: Camera, lights: Light list) = 
    let shapes = shapes
    let camera = camera
    let lights = lights
    let backgroundColour = new Colour(0.,0.,0.)
    member this.Shapes = shapes
    member this.Camera = camera
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour   
    member this.Render = 

        // Prepare image
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)
            
        // Field of view
        let n = (camera.Position - camera.Lookat).Normalise
        let v = (n % camera.Up).Normalise
        let hfov = Math.PI/3.5
        let vfov = hfov * float(camera.ResY)/float(camera.ResX)
        let pw = 2.0 * tan(float(hfov/2.0))/float(camera.ResX)
        let ph = 2.0 * tan(float(vfov/2.0))/float(camera.ResY)
            
        // Shoot rays and render image
        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                let rayOrigin = camera.Position - n + float(x-camera.ResX/2)*pw*camera.Up + float(y-camera.ResY/2)*ph*v
                let rayDirection = (rayOrigin - camera.Position).Normalise
                let ray = new Ray(camera.Position,rayDirection)

                let mutable tSmallest = infinity 
                let mutable normal = Vector(0., 0., 0.)
                let mutable colour = backgroundColour

                //printfn "%A" ray.GetOrigin.Z
                //printfn "%A" ray.GetDirection.Z

                //printfn "%A" (-(ray.GetOrigin.Z) / (ray.GetDirection.Z))

                //if (-(ray.GetOrigin.Z) / (ray.GetDirection.Z)) > 0.0  then printfn "ray shoots toward 2D shape and crosses the XY Plane" else printfn "misses the 2D shapes"

                //-(ray.GetOrigin.Z / ray.GetDirection.Z)
                //&& ray.GetDirection.Z <> 0.0

                //printfn "%A" ray.GetDirection.X
                (*
                for s in shapes do
                    match s.hitFunction ray with
                    |(None,None,None) ->  tSmallest |> ignore
                    |(t,d,m) when t.Value < tSmallest -> tSmallest <- t.Value
                                                         normal <- d.Value
                                                         colour <- m.Value.AmbientColour
                    |(t,d,m) -> tSmallest |> ignore
                    //shoot ray at all shapes, if t < tsmallest, update the mutable values. 
                    //might want to do this without the mutable values
                //let colour = ray.Cast backgroundColour lights shapes
                *)
                renderedImage.SetPixel(x, y, colour.ToColor)

        // Save image
        renderedImage.Save(camera.RenderFilepath)
        
        // Open image
        Process.Start(camera.RenderFilepath)
        