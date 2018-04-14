namespace Tracer.Basics
open System

type Sphere(origin: Point, radius: float, material: Material) = 
    let origin = origin
    let radius = radius
    let material = material
    member this.Origin = origin
    member this.Radius = radius
    member this.Material = material
    member this.NormalAtPoint (p:Point) = 
        (p - origin).Normalise
    member this.GetDiscriminant (ray:Ray) = 
        let s = (ray.GetOrigin - origin)
        let rayDir = ray.GetDirection.Normalise
        let sv = s * rayDir
        let ss = s * s
        sv*sv - ss + radius * radius
    member this.GetHitPoints (ray:Ray) = 
        let D = this.GetDiscriminant ray
        if D < 0. then
            invalidArg "ray" "ray did not hit, so no hitpoints can be returned"
        else
            let s = (ray.GetOrigin - origin)
            let rayDir = ray.GetDirection.Normalise
            let sv = s * rayDir
            let ss = s * s
            let (t1,t2) = (-sv + Math.Sqrt(D), -sv - Math.Sqrt(D))
            (ray.PointAtTime t1,ray.PointAtTime t2)