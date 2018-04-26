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
        let rayOrigin = base.Vpc + (float(x)-this.Width/2.) * base.Pw * base.U + float(float(y)-base.Height/2.)*base.Ph*base.V
        let rayDirection = (rayOrigin - base.Position).Normalise
        let ray = new Ray(base.Position,rayDirection)
        let colour = ray.Cast bgColor lights shapes
        ()