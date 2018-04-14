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