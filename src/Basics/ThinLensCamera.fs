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
    default this.CreateRay x y =
        // Create Ray, setup direction and origin.
        let qx, qy = viewSamples.Next() // Sample unit square for center point.
        let qx = (float(x)-qx) * base.Pw
        let qy = float(float(y)-qy) * base.Ph
        let px, py = (f * qx)/zoom, (f * qy)/zoom
        let lx, ly = lensSamples.Next() // Sample unit disc with respect to r.
        let lx = lx * r
        let ly = ly * r

        (*Not sure if we need this?
        let rayOrigin = base.Vpc + (float(x)-this.Width/2.) * base.Pw * base.U + float(float(y)-base.Height/2.)*base.Ph*base.V
        let rayDirection = (rayOrigin - base.Position).Normalise
        let ray = new Ray(base.Position,rayDirection)*)

        // Create the primary lens ray, 
        // from the lens disc point to the focal unit square point.
        let rayOrigin = base.Position + lx * base.U + ly * base.V
        let rayDirection = ((px - lx) * base.U + (py - ly) * base.V - f * base.W).Normalise
        new Ray(rayOrigin, rayDirection)