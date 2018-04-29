namespace Tracer.Basics

type Ray(origin: Point, direction: Vector) = 
    member this.GetOrigin = origin
    member this.GetDirection = direction.Normalise

    // Returns a point from a given time/length of the ray
    member this.PointAtTime (t:float) = 
        origin + t * direction

    // Returns a time/length from a given point of the ray
    member this.TimeAtPoint (p:Point) = 
        (p - origin).Z / direction.Z

    member this.Invert = 
        Ray(origin, direction.Invert)

    static member None = 
        Ray(Point(0.,0.,0.),Vector(0.,0.,0.))