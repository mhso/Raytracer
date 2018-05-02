﻿namespace Tracer.Basics

open System.Drawing

type PinholeCamera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point, up: Vector, zoom: float, width: float, height: float, resX: int, resY: int) =
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)
    member this.RenderFilepath = "background.bmp"

    default this.CreateRays x y =
        let rayTarget = base.Vpc + (float(x)-base.Width/2.) * base.Pw * base.V + float(float(y)-base.Height/2.)*base.Ph*base.U
        let rayDirection = (rayTarget - base.Position).Normalise
        [new Ray(base.Position,rayDirection)]