namespace Tracer.Basics

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
        viewSamples : unit,
        lensSamples : unit
    ) = 
    inherit Camera(position, lookat, up, zoom, width, height, resX, resY)    
    member this.Cast x y bgColor lights shapes =
        // Create Ray, setup direction and origin.
        let qx, qy = 0.0, 0.0 // Sample unit square with respect to x and y
        let squarePoint = new Point(qx, qy, -zoom)
        let px, py = (f * qx)/zoom, (f * qy)/zoom
        let lx, ly = 0.0*r, 0.0*r // Sample unit disc with respect to r.

        (* Not sure if we need this?
        let rayOrigin = base.Vpc + (float(x)-this.Width/2.) * base.Pw * base.U + float(float(y)-base.Height/2.)*base.Ph*base.V
        let rayDirection = (rayOrigin - base.Position).Normalise
        let ray = new Ray(base.Position,rayDirection)*)

        // Create the primary lens ray, 
        // from the lens disc point to the focal unit square point.
        let rayOrigin = position + lx * base.U + ly * base.V
        let rayDirection = ((px - lx) * base.U + (py - ly) * base.V - f * base.W).Normalise
        let ray = new Ray(rayOrigin, rayDirection)

        // Cast the ray.
        let colour = ray.Cast bgColor lights shapes
        ()