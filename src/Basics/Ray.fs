namespace Tracer.Basics

type Ray(origin: Point, direction: Vector) = 
    
    //- PRIVATE FIELDS
    let origin = origin
    let direction = direction

    //- PUBLIC FIELDS
    member this.GetOrigin = origin
    member this.GetDirection = direction
    member this.PointAtTime (t:float) = 
        origin + t * direction
    member this.TimeAtPoint (p:Point) = 
        (p - origin).Z / direction.Z
    member this.PerfectReflect (camera: Camera) (hitPoint: Point) (normal: Vector) =
        let newOrigin = hitPoint
        let newDirection = -direction + (-2. * (normal * -direction)) * normal
        new Ray(newOrigin, newDirection)
