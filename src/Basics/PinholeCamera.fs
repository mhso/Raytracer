﻿namespace Tracer.Basics

open Tracer.Basics.Sampling

type PinholeCamera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point,
                    up: Vector, zoom: float, width: float, height: float, 
                    resX: int, resY: int, sampler : Sampler) =
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)

    default this.CreateRays x y =
        let samples = sampler.NextSet()
        [|for (sx, sy) in samples do
            let px = this.Pw * (float(x - (resX/2)) + sx)
            let py = this.Ph * (float(y - (resY/2)) + sy)
            let pz = -zoom
            let direction = (px * this.V) + (py * this.U) + (pz * this.W)
            yield (new Ray(position, direction.Normalise))|]