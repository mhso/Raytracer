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
        (sv*sv) - ss + (radius*radius)
    member this.GetHitPoint (ray:Ray) = 
        let D = this.GetDiscriminant ray
        if D < 0. then
            new HitPoint(ray)
        else
            let s = (ray.GetOrigin - origin.ToVector).ToVector
            let rayDir = ray.GetDirection.Normalise
            let sv = s * rayDir
            let (t1,t2) = (-sv + Math.Sqrt(D), -sv - Math.Sqrt(D))
            let p = ray.PointAtTime (if t1 <= t2 then t1 else t2)
            let a = new HitPoint(ray, if t1 <= t2 then t1 else t2)
            a
    static member None = 
        new Sphere(new Point(0.,0.,0.), 0., new MatteMaterial(Colour.Black))