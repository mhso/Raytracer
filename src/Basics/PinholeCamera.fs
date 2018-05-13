namespace Tracer.Basics

open Tracer.Sampling.Sampling

type PinholeCamera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point,
                    up: Vector, zoom: float, width: float, height: float, 
                    resX: int, resY: int, sampler : Sampler) =
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)

    // Reuse array for better performance.
    let reuseRayArr = Array.zeroCreate sampler.SampleCount

    default this.CreateRays x y =
        for i in 0..sampler.SampleCount-1 do
            let sx, sy = if sampler.SampleCount > 1 then sampler.Next() else 0.5, 0.5
            let px = this.Pw * (float(x - (resX/2)) + sx)
            let py = this.Ph * (float(y - (resY/2)) + sy)
            let pz = -zoom
            let direction = (px * this.V) + (py * this.U) + (pz * this.W)
            reuseRayArr.[i] <- (new Ray(position, direction.Normalise))
        reuseRayArr