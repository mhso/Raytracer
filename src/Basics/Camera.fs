namespace Tracer.Basics

open System.Drawing
open System

[<AbstractClass>]
type Camera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point, up: Vector, zoom: float, width: float, height: float, resX: int, resY: int) =
    // Field of view and orthonormal coordinate system.
    let w = (position - lookat).Normalise
    let v = up % w
    let u = -(w % v)
    let pw = width/float resX
    let ph = height/float resY
    let viewOffset = position - w

    member this.W = w
    member this.U = u
    member this.V = v
    member this.Pw = pw
    member this.Ph = ph
    member this.Position = position
    member this.Lookat = lookat
    member this.Up = up
    member this.Zoom = zoom
    member this.Width = width
    member this.Height = height
    member this.ResX = resX
    member this.ResY = resY
    member this.RenderFilepath = "background.bmp"
    member this.Direction = 
        (lookat - position).Normalise

    abstract member CreateRays : int -> int -> Ray list

    member this.Cast ray bgColor (shapes : Shape []) (lights : Light list) =
        // Get the hitpoint
        let (shape, (hitPoint: HitPoint)) = this.GetFirstHitPoint ray shapes

        // Check if we hit
        if hitPoint.DidHit then
            // Sum the light colors for that hitpoint
            let normal = hitPoint.Normal
            lights 
                |> List.fold (fun accColour light -> 
                    let shadowColour = this.CastShadow hitPoint light shapes
                    let colour = this.CastRecursively ray shape hitPoint light Colour.Black hitPoint.Material.Bounces shapes hitPoint.Material.BounceMethod
                    accColour + (colour - shadowColour)) (new Colour(0.,0.,0.))
        else
            // If we did not hit, return the background colour
            bgColor

    // Get the first point the ray hits (if it hits, otherwise an empty hit point)
    member this.GetFirstHitPoint (ray:Ray) (shapes : Shape []) = 

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

    member this.GetFirstShadowHitPoint (ray:Ray) (shapes:Shape []) = 
        
        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction ray )]
                |> List.filter (fun (_,hp:HitPoint) -> hp.DidHit)
                |> List.filter (fun (_,hp:HitPoint) -> not (hp.Material :? EmissiveMaterial)) // Filter out emisive materials
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape.None, HitPoint(ray))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)


    member this.GetFirstHitPointExcept (ray: Ray) (shapes: Shape []) (except: Shape) = 

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

    // Returns the average shadow for a hitpoint and a light source
    member this.CastShadow (hitPoint: HitPoint) (light: Light) (shapes : Shape []) = 
        if light :? AmbientLight 
            then Colour.Black
        else
            let shadowRays = light.GetShadowRay hitPoint
            let isShadow ray = 
                let (_, hp) = (this.GetFirstShadowHitPoint ray shapes)
                if hp.DidHit then
                        Colour.White 
                    else 
                        Colour.Black
            
            if shadowRays.Length = 0 then 
                Colour.Black
            else
                let totalShadow = Array.fold (fun acc ray -> acc + isShadow ray) Colour.Black shadowRays
                (totalShadow / float(shadowRays.Length))

    // Will cast a ray recursively
    member this.CastRecursively 
        (incomingRay: Ray) (shape: Shape) (hitPoint: HitPoint) (light: Light) (acc: Colour) (bounces: int)
        (shapes : Shape []) (reflectionFunction: HitPoint -> Ray[]) =
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
                        this.CastRecursively outRay.[i] outShape outHitPoint light baseColour (bounces - 1) shapes reflectionFunction
                    else
                        Colour.Black
            baseColour + (outColour / float(outRay.Length))