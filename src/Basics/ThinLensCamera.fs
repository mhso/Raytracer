namespace Tracer.Basics

open Tracer.Sampling.Sampling

type ThinLensCamera
    (
        position : Point, 
        lookat : Point,
        up: Vector,
        zoom: float,
        width: float,
        height: float,
        resX: int,
        resY: int,
        r: float,
        f: float,
        viewSamples : Sampler,
        lensSamples : Sampler
    ) = 
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)    
    default this.CreateRays x y =
        let rays = Array.zeroCreate lensSamples.SampleCount
        for i in 0..lensSamples.SampleCount-1 do
            // Create Ray, setup direction and origin.
            let qx, qy = viewSamples.Next() // Sample unit square for center ray.
            let qx = base.Pw * ((float x - float resX/2.0) + qx)
            let qy = base.Ph * ((float y - float resY/2.0) + qy)
            
            let px, py = (f * qx)/zoom, (f * qy)/zoom

            let lx, ly = lensSamples.Next() // Sample unit disc with respect to r.
            let lx = lx * r
            let ly = ly * r

            // Create the primary lens ray, 
            // from the lens disc point to the focal point.
            let rayOrigin = base.Position + lx * base.V + ly * base.U

            let rayDirection = ((px - lx) * base.V + (py - ly) * base.U - f * base.W).Normalise
            rays.[i] <- (new Ray(rayOrigin, rayDirection))
        rays