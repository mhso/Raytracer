namespace Tracer.Basics

open System.Drawing
open System

type PinholeCamera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point, up: Vector, zoom: float, width: float, height: float, resX: int, resY: int) =
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)
    member this.RenderFilepath = "background.bmp"

    default this.CreateRay x y =
        let rayOrigin = base.Vpc + (float(x)-base.Width/2.) * base.Pw * base.U + float(float(y)-base.Height/2.)*base.Ph*base.V
        let rayDirection = (rayOrigin - base.Position).Normalise
        new Ray(base.Position,rayDirection)