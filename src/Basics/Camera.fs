namespace Tracer.Basics

open System.Drawing
open System

[<AbstractClass>]
type Camera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point, up: Vector, zoom: float, width: float, height: float, resX: int, resY: int) =
    // Field of view and orthonormal coordinate system.
    let w = (position - lookat).Normalise
    let v = up % w
    let u = -(w % v) // <-- HACK ALERT!! We invert y because otherwise up would be negative and down would be positive.
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

    