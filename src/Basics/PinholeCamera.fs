﻿namespace Tracer.Basics

type PinholeCamera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point, up: Vector, zoom: float, width: float, height: float, resX: int, resY: int) =
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)
    member this.RenderFilepath = "background.bmp"

    default this.CreateRays x y =
        let px = this.Pw * ((float x - float resX/2.0) + 0.5)
        let py = this.Ph * ((float y - float resY/2.0) + 0.5)
        let p = new Point(px, py, zoom)
        let direction = (px * this.V) + (py * this.U) - (zoom * this.W)

        [new Ray(this.Position, direction.Normalise)]