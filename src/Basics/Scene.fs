namespace Tracer.Basics

open System
open System.Drawing
open System.Diagnostics

type Scene(shapes: Shape list, camera: Camera, lights: Light list) = 

    let backgroundColour = new Colour(0., 0., 0.)
    member this.Spheres = shapes
    member this.Camera = camera
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour   
        
    // Get the first point the ray hits (if it hits, otherwise an empty hit point)
    member this.GetFirstHitPoint (ray:Ray) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction ray )]
                |> List.filter (fun (_,hp:HitPoint) -> hp.DidHit)
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape.None, HitPoint(ray))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

    member this.GetFirstHitPointExcept (ray: Ray) (shapes: Shape list) (except: Shape) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction ray)]
                |> List.filter (fun (_,hp) -> hp.DidHit)
                |> List.filter (fun (shape,_) -> 
                    let eq = Object.ReferenceEquals(shape, except)
                    not eq)

        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape.None, new HitPoint(ray))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

    // Returns the colour in the first hit point of the ray
    member this.Cast (ray: Ray) = 

        // Get the hitpoint
        let (shape, hitPoint) = this.GetFirstHitPoint ray

        // Check if we hit
        if hitPoint.DidHit then
            // Sum the light colors for that hitpoint
            let normal = hitPoint.Normal
            lights 
                |> List.fold (fun accColour light -> 
                    let shadowColour = this.CastShadow hitPoint light
                    let colour = this.CastRecursively ray shape hitPoint light Colour.Black hitPoint.Material.Bounces hitPoint.Material.BounceMethod
                    accColour + (colour - shadowColour)) (new Colour(0.,0.,0.))
        else
            // If we did not hit, return the background colour
            backgroundColour

    // Returns the average shadow for a hitpoint and a light source
    member this.CastShadow (hitPoint: HitPoint) (light: Light) = 
        if light :? AmbientLight 
            then Colour.Black
        else
            let shadowRays = light.GetShadowRay hitPoint
            let isShadow ray = 
                let (_, hp) = (this.GetFirstHitPoint ray)
                if hp.DidHit then Colour.White else Colour.Black
            let totalShadow = Array.fold (fun acc ray -> isShadow ray) Colour.Black shadowRays
            (totalShadow / float(shadowRays.Length))

    // Will cast a ray recursively
    member this.CastRecursively 
        (incomingRay: Ray) (shape: Shape) (hitPoint: HitPoint) (light: Light) (acc: Colour) (bounces: int)
        (reflectionFunction: HitPoint -> Ray[]) =
        if bounces = 0 || hitPoint.Material.Bounces = 0 then
            acc + hitPoint.Material.PreBounce shape hitPoint light
        else
            let outRay = reflectionFunction hitPoint
            let baseColour = acc + hitPoint.Material.PreBounce shape hitPoint light
            let mutable outColour = Colour.Black
            for i = 0 to outRay.Length-1 do
                outColour <- outColour + 
                    let (outShape, outHitPoint) = this.GetFirstHitPointExcept outRay.[i] shapes shape
                    if outHitPoint.DidHit then
                        this.CastRecursively outRay.[i] outShape outHitPoint light baseColour (bounces - 1) reflectionFunction
                    else
                        Colour.Black
            baseColour + (outColour / float(outRay.Length))
            
    member this.Render = 

        // Prepare image
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)
            
        // Field of view
        let n = (camera.Position - camera.Lookat).Normalise
        let u = (camera.Up % n)
        let v = (n % u)
        let hfov = Math.PI/3.5
        let vfov = hfov * float(camera.ResY)/float(camera.ResX)
        let pw = 2.0 * tan(float(hfov/2.0))/float(camera.ResX)
        let ph = 2.0 * tan(float(vfov/2.0))/float(camera.ResY)
        let vpc = camera.Position - n

        // Shoot rays and render image
        let total = float (camera.ResX * camera.ResY)
        let mutable currPct = 0

        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                let rayOrigin = vpc + (float(x)-camera.Width/2.) * pw * u + float(float(y)-camera.Height/2.)*ph*v
                let rayDirection = (rayOrigin - camera.Position).Normalise
                let ray = new Ray(camera.Position,rayDirection)
                let colour = this.Cast ray
                
                (*
                let pct = int((float (x*y)/total) * 100.0)

                if (pct/5) > currPct then
                    Console.Clear()
                    printf "Progress: |"
                    currPct <- pct/5
                    let dots = String.replicate currPct "█"
                    let white = String.replicate (20-currPct) "░"
                    printf "%s" (dots + white)
                    printf "%s"  ("| " + string pct + "%")*)
        
                renderedImage.SetPixel(x, y, colour.ToColor)

        
        // Save image
        renderedImage.Save(camera.RenderFilepath)
        
        // Open image
        Process.Start(camera.RenderFilepath)

    
        