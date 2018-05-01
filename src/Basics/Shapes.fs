namespace Tracer.Basics
open System

type Rectangle(bottomLeft:Point, topLeft:Point, bottomRight:Point, tex:Material)=
    inherit Shape()
    member this.bottomleft = bottomLeft
    member this.topLeft = topLeft
    member this.bottomRight = bottomRight
    member this.tex = tex
    member this.width = bottomRight.X - bottomLeft.X
    member this.height = topLeft.Y - bottomLeft.Y
    member this.normal:Vector = new Vector(0.0, 0.0, 1.0)
    default this.hitFunction (r:Ray) = 
        match r with
            |(r) when (r.GetDirection.Z) = 0.0 -> HitPoint(r) //This method checks if dz = 0.0, which would make the ray, parrallel to the plane 
            |(r) when (-((r.GetOrigin.Z) / (r.GetDirection.Z))) <= 0.0 -> HitPoint(r) //This checks if t is 0 or smaller, in which case there is no hit
            |(r) -> let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z)))
                    let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                    let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                    if (px > 0.0 && px < this.width) && (py > 0.0 && py < this.height) 
                    then HitPoint(r, t, this.normal, tex) else HitPoint(r)


                                                        
type Disc(center:Point, radius:float, tex:Material)=
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.tex = tex
    member this.normal: Vector = new Vector(0.0, 0.0, 1.0)
    default this.hitFunction (r:Ray) = 
        match r with
            |(r) when (r.GetDirection.Z) = 0.0 -> HitPoint(r) //This method checks if dz = 0.0, which would make the ray, parrallel to the plane 
            |(r) when (-((r.GetOrigin.Z) / (r.GetDirection.Z))) <= 0.0 -> HitPoint(r) //This checks if t is 0 or smaller, in which case there is no hit
            |(r) -> let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z)))
                    let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                    let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                    if (((px*px)+(py*py)) <= radius*radius) 
                    then HitPoint(r, t, this.normal, tex) else HitPoint(r)
    



and Triangle(a:Point, b:Point, c:Point, mat:Material)=
    inherit Shape()
    member this.a = a
    member this.b = b
    member this.c = c
    member this.mat = mat
    member this.u = a-b //in case of errors try swithing a and b around
    member this.v = a-c // same here

    //the many let statements are fo simplifying cramers rule
    default this.hitFunction (r:Ray) = 
        let pa = ((a.X)-(b.X))
        let pb = ((a.X)-(c.X))
        let e = ((a.Y)-(b.Y))
        let f = ((a.Y)-(c.Y))
        let i = ((a.Z)-(b.Z))
        let j = ((a.Z)-(c.Z))

        match r with
            |(r) when (pa*((f*(r.GetDirection.Z))-((r.GetDirection.Y)*j)) + pb*(((r.GetDirection.Y)*i)-(e*(r.GetDirection.Z))) + (r.GetDirection.X)*((e*j)-(f*i))) = 0.0 -> HitPoint(r)
            |(r) -> let pc = (r.GetDirection.X)
                    let g = (r.GetDirection.Y)
                    let k = (r.GetDirection.Z)
                    let d = ((a.X)-(r.GetOrigin.X)) 
                    let h = ((a.Y)-(r.GetOrigin.Y)) 
                    let l = ((a.Z)-(r.GetOrigin.Z))
                    let D = (pa*((f*k)-(g*j)) + pb*((g*i)-(e*k)) + pc*((e*j)-(f*i)))
                    let x = (d*((f*k)-(g*j)) + pb*((g*l)-(h*k)) + pc*((h*j)-(f*l)))/D
                    let y = (pa*((h*k)-(g*l)) + d*((g*i)-(e*k)) + pc*((e*l)-(h*i)))/D
                    let z = (pa*((f*l)-(h*j)) + pb*((h*i)-(e*l)) + d*((e*j)-(f*i)))/D
                    //x=beta, y=gamma, z=t
                    //alpha is gained from 1-x-y, this is used for texturing (alpha, beta, gamma that is)
                    if (x <= 1.0 && x >= 0.0) && (y <= 1.0 && y >= 0.0) && (x+y <= 1.0 && x+y >= 0.0) && (z>0.0)
                            then HitPoint(r, z, (this.u % this.v).Normalise, mat) else HitPoint(r)
                            

type SphereShape(origin: Point, radius: float, tex: Material) = 
    inherit Shape()
    member this.Origin = origin //perhaps both should be lower case
    member this.Radius = radius
    member this.tex = tex
    member this.NormalAtPoint (p:Point) = 
        (p - origin).Normalise
    member this.GetDiscriminant (ray:Ray) = 
        let s = (ray.GetOrigin - origin)
        let rayDir = ray.GetDirection.Normalise
        let sv = s * rayDir
        let ss = s * s
        sv*sv - ss + radius * radius

    default this.hitFunction (r:Ray) = 
        let D = this.GetDiscriminant r
        if D < 0. then HitPoint(r)
        else
            let s = (r.GetOrigin - origin)
            let rayDir = r.GetDirection.Normalise
            let sv = s * rayDir
            let (t1,t2) = (-sv + Math.Sqrt(D), -sv - Math.Sqrt(D))
            if t1 < 0. && t2 < 0. then HitPoint(r)
            elif t1 < t2 then HitPoint(r, t1, this.NormalAtPoint (r.PointAtTime t1), tex)
            else HitPoint(r, t2, this.NormalAtPoint (r.PointAtTime t2), tex)



type HollowCylinder(center:Point, radius:float, height:float, tex:Material) = //change back to texture
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    member this.tex = tex

    member this.determineHitPoint (r:Ray) (t:float) = 
        let p = r.PointAtTime t
        if p.Y > -(height/2.0) && p.Y < (height/2.0) then HitPoint(r, t, Vector(p.X/radius, 0.0, p.Z/radius), tex) else HitPoint (r)

    default this.hitFunction (r:Ray) = 
        let a = ((r.GetDirection.X)**2.0) + ((r.GetDirection.Z)**2.0) //both are to the power of 2
        let b = 2.0*((r.GetOrigin.X * r.GetDirection.X)+(r.GetOrigin.Z * r.GetDirection.Z))
        let c = ((r.GetOrigin.X)**2.0) + ((r.GetOrigin.Z)**2.0) - (radius**2.0)
        let D = (b**2.0) - 4.0*a*c
        let t1 = (-b + Math.Sqrt(D))/(2.0 * a)
        let t2 = (-b - Math.Sqrt(D))/(2.0 * a)
        match D with
        |(0.0) -> match (t1,t2) with
                  |(t1,t2) when t1 <= 0.0 && t2 <= 0.0 -> HitPoint(r)
                  |(t1,t2) -> if t1 < t2 && t1 > 0.0 then this.determineHitPoint r t1 else this.determineHitPoint r t2
        |(D) when D < 0.0 -> HitPoint(r)
        |(D) -> match (t1,t2) with //when D > 0.0, and there are two valid values for t
                  |(t1,t2) when t1 <= 0.0 && t2 <= 0.0 -> HitPoint(r)
                  |(t1,t2) -> if t1 < t2 && t1 > 0.0 then this.determineHitPoint r t1 else  if t2 > 0.0 then this.determineHitPoint r t2 
                                                                                            else this.determineHitPoint r t1

type SolidCylinder(center:Point, radius:float, height:float, cylinder:Material, top:Material, bottom:Material) =
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    member this.cylinder = cylinder
    member this.top = top
    member this.bottom = bottom

    default this.hitFunction (r:Ray) = HitPoint(r)
    //affine transformation is needed for moving the disks


type Box(low:Point, high:Point, front:Material, back:Material, top:Material, bottom:Material, left:Material, right:Material) = 
    inherit Shape()
    member this.low = low
    member this.high = high
    member this.front = front
    member this.back = back
    member this.top = top
    member this.bottom = bottom
    member this.left = left
    member this.right = right

    default this.hitFunction (r:Ray) = 
        let tx = if r.GetDirection.X >= 0.0 then (low.X - r.GetOrigin.X)/r.GetDirection.X else (high.X - r.GetOrigin.X)/r.GetDirection.X
        let tx' = if r.GetDirection.X >= 0.0 then (high.X - r.GetOrigin.X)/r.GetDirection.X else (low.X - r.GetOrigin.X)/r.GetDirection.X
        let ty = if r.GetDirection.Y >= 0.0 then (low.Y - r.GetOrigin.Y)/r.GetDirection.Y else (high.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let ty' = if r.GetDirection.Y >= 0.0 then (high.Y - r.GetOrigin.Y)/r.GetDirection.Y else (low.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let tz = if r.GetDirection.Z >= 0.0 then (low.Z - r.GetOrigin.Z)/r.GetDirection.Z else (high.Z - r.GetOrigin.Z)/r.GetDirection.Z
        let tz' = if r.GetDirection.Z >= 0.0 then (high.Z - r.GetOrigin.Z)/r.GetDirection.Z else (low.Z - r.GetOrigin.Z)/r.GetDirection.Z
        

        let t = max tx (max ty tz)

        let t' = min tx' (min ty' tz')

        if t < t' && t' > 0.0 then 
            if t > 0.0 then 
                match (tx, ty, tz) with
                |(tx,ty,tz) when tx >= ty && tx >= tz -> if r.GetDirection.X > 0.0 then HitPoint(r, t, Vector(-1.0, 0.0, 0.0), left) //when tx is the biggest and t > 0.0
                                                         else HitPoint(r, t, Vector(1.0,0.0,0.0), right)
                |(tx,ty,tz) when ty >= tx && ty >= tz -> if r.GetDirection.Y > 0.0 then HitPoint(r, t, Vector(0.0, -1.0, 0.0), bottom) //when ty is the biggest and t > 0.0
                                                         else HitPoint(r, t, Vector(0.0, 1.0, 0.0), top)
                |(tx,ty,tz) when tz >= tx && tz >= ty -> if r.GetDirection.Z > 0.0 then HitPoint(r, t, Vector(0.0, 0.0, -1.0), back) //when tz is the biggest and t > 0.0
                                                         else HitPoint(r, t, Vector(0.0, 0.0, 1.0), front)
            else
                match (tx', ty', tz') with
                |(tx',ty',tz') when tx' <= ty' && tx' <= tz' -> if r.GetDirection.X > 0.0 then HitPoint(r, t, Vector(1.0, 0.0, 0.0), right) //when tx' is the smallest and t > 0.0
                                                                else HitPoint(r, t, Vector(-1.0, 0.0, 0.0), back)
                |(tx',ty',tz') when ty' <= tx' && ty' <= tz' -> if r.GetDirection.Y > 0.0 then HitPoint(r, t, Vector(0.0, 1.0, 0.0), top) //when ty' is the smallest and t > 0.0
                                                                else HitPoint(r, t, Vector(0.0, -1.0, 0.0), bottom)
                |(tx',ty',tz') when tz' <= tx' && tz' <= ty' -> if r.GetDirection.Z > 0.0 then HitPoint(r, t, Vector(0.0, 0.0, 1.0), front) //when tz' is the smallest and t > 0.0
                                                                else HitPoint(r, t, Vector(0.0, 0.0, -1.0), back)
        else HitPoint(r)
        

type InfinitePlane(tex:Material) = 
    inherit Shape()
    member this.tex = tex
    default this.hitFunction (r:Ray) = 
        let t = -(r.GetOrigin.Z / r.GetDirection.Z)
        if r.GetDirection.Z <> 0.0 && t > 0.0 then HitPoint(r, t, Vector(0.0, 0.0, 1.0), tex) else HitPoint(r)

type TransformShape (hitFunction) =
    inherit Shape()
    default this.hitFunction(r: Ray) = hitFunction r