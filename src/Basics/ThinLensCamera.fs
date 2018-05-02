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
        viewSamples : SampleGenerator,
        lensSamples : SampleGenerator
    ) = 
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)    
    default this.CreateRays x y =
        let mutable rays = []
        for i in 0..lensSamples.SampleCount-1 do
            // Create Ray, setup direction and origin.
            let qx, qy = viewSamples.Next() // Sample unit square for center point.
            let qx = qx + float x
            let qy = qy + float y
            let px, py = (f * qx)/zoom, (f * qy)/zoom
            let lx, ly = lensSamples.Next() // Sample unit disc with respect to r.
            let lx = lx * r
            let ly = ly * r

            //let rayOrigin = base.Vpc + (qx + float(x)-base.Width/2.) * base.Pw * base.V + (qy + float(y)-base.Height/2.)*base.Ph*base.U
            //let rayDirection = (rayOrigin - base.Position).Normalise
            //rays <- (new Ray(base.Position, rayDirection))::rays

            // Create the primary lens ray, 
            // from the lens disc point to the focal unit square point.
        

            let rayOrigin = base.Vpc + lx * base.U + ly * base.V
            let rayTarget = base.Vpc + (qx + float(x)-base.Width/2.) * base.Pw * base.V + (qy + float(y)-base.Height/2.)*base.Ph*base.U

            let rayDirection = ((px - lx) * base.U + (py - ly) * base.V - f * base.W).Normalise
            rays <- (new Ray(rayOrigin, rayDirection))::rays
        rays