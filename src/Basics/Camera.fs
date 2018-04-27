namespace Tracer.Basics

open System.Drawing
open System

[<AbstractClass>]
type Camera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point, up: Vector, zoom: float, width: float, height: float, resX: int, resY: int) =
    let w = (position - lookat).Normalise
    let hfov = Math.PI/3.5
    let vfov = hfov * float(resY)/float(resX)
    let u = up % w
    let v = w % u
    let pw = 2.0 * tan(float(hfov/2.0))/float(resX)
    let ph = 2.0 * tan(float(vfov/2.0))/float(resY)
    let vpc = position - w

    member this.W = w
    member this.U = u
    member this.V = v
    member this.Pw = pw
    member this.Ph = ph
    member this.Vpc = vpc
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
    // Cast recurve
    (*member this.GetFirstHitPoint (shapes: Shape list) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction this)]
                |> List.filter (fun (_,hp:HitPoint) -> hp.DidHit)
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape(), new HitPoint(this))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

     member this.GetFirstHitPointExcept (shapes: Shape list) (except: Shape) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction this)]
                |> List.filter (fun (_,hp) -> hp.DidHit)
                |> List.filter (fun (shape,_) -> 
                    let eq = Object.ReferenceEquals(shape, except)
                    not eq)

        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape(), new HitPoint(this))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)
    abstract member Cast: int -> int -> Colour -> Light list -> Shape list -> Colour*)