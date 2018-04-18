namespace Tracer.Basics

type HitPoint(ray: Ray, time: float, didHit: bool) = 
    
    member this.Ray = ray
    member this.Time = time
    member this.Point = 
        ray.PointAtTime time
    member this.DidHit = didHit

    new(ray: Ray, time:float) = HitPoint(ray, time, true)
    new(ray: Ray) = HitPoint(ray, 0., false)