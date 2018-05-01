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
    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...
    override this.getBoundingBox () = 
        let e = 0.000001
        let lx = (min bottomLeft.X (min topLeft.X bottomRight.X)) - e
        let ly = (min bottomLeft.Y (min topLeft.Y bottomRight.Y)) - e
        let lz = (min bottomLeft.Z (min topLeft.Z bottomRight.Z)) - e //might be redundant as Z should always equal 0

        let hx = (max bottomLeft.X (max topLeft.X bottomRight.X)) - e
        let hy = (max bottomLeft.Y (max topLeft.Y bottomRight.Y)) - e
        let hz = (max bottomLeft.Z (max topLeft.Z bottomRight.Z)) - e //might be redundant as Z should always equal 0

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    //override this.getTextureCoords (p:Point) = ((p.X / this.width), (p.Y / this.height))
    override this.hitFunction (r:Ray) = 
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
    member this.radius = radius // must not be a negative number
    member this.tex = tex
    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...
    override this.getBoundingBox () =  //no point on the disc should be larger than the center point + the radius...
        let e = 0.000001
        let lx = (center.X - radius) - e
        let ly = (center.Y - radius) - e
        let lz = 0.0 - e

        let hx = (center.X + radius) + e
        let hy = (center.Y + radius) + e
        let hz = 0.0 + e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    (*
    override this.getTextureCoords (p:Point) = 
        let u = (p.X + radius)/(2.*radius)
        let v = (p.Y + radius)/(2.*radius)
        (u, v)
    *)

    member this.normal: Vector = new Vector(0.0, 0.0, 1.0)
    override this.hitFunction (r:Ray) = 
        match r with
            |(r) when (r.GetDirection.Z) = 0.0 -> HitPoint(r) //This method checks if dz = 0.0, which would make the ray, parrallel to the plane 
            |(r) when (-((r.GetOrigin.Z) / (r.GetDirection.Z))) <= 0.0 -> HitPoint(r) //This checks if t is 0 or smaller, in which case there is no hit
            |(r) -> let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z)))
                    let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                    let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                    if (((px*px)+(py*py)) <= radius*radius) 
                        then 
                            let u = (px + radius)/(2.*radius)
                            let v = (py + radius)/(2.*radius)
                            HitPoint(r, t, this.normal, tex) 
                    else HitPoint(r)
                    
    



and Triangle(a:Point, b:Point, c:Point, mat:Material)=
    inherit Shape()
    member this.a = a
    member this.b = b
    member this.c = c
    member this.mat = mat
    member this.u = a-b //in case of errors try swithing a and b around
    member this.v = a-c // same here

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...
    override this.getBoundingBox () = 
        let e = 0.000001
        let lx = (min a.X (min b.X c.X)) - e
        let ly = (min a.Y (min b.Y c.Y)) - e
        let lz = (min a.Z (min b.Z c.Z)) - e //might be redundant as Z should always equal 0

        let hx = (max a.X (max b.X c.X)) - e
        let hy = (max a.Y (max b.Y c.Y)) - e
        let hz = (max a.Z (max b.Z c.Z)) - e //might be redundant as Z should always equal 0

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    //no method for returning texture coords are given by the lecture notes....
    //seems to be under triangle meshes (perhaps texturing)
    //override this.getTextureCoords (p:Point) = 
        //let uCoord =
        //let vCoord =
        //(uCoord, vCoord)
        //(0., 0.)

    //the many let statements are for simplifying cramers rule
    override this.hitFunction (r:Ray) = 
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
                            then HitPoint(r, z, (this.u % this.v).Normalise, mat) else HitPoint(r) //why mat instead of texture
                            

type SphereShape(origin: Point, radius: float, tex: Material) = 
    inherit Shape()
    member this.Origin = origin //perhaps both should be lower case
    member this.Radius = radius
    member this.tex = tex

    override this.isInside (p:Point) =
        let x = (p.X - origin.X)**2. + (p.Y - origin.Y)**2. + (p.Z - origin.Z)**2.
        if x < (radius**2.) then true else false
    override this.getBoundingBox () =  //no point on the sphere should be larger than the center point + the radius...
        let e = 0.000001
        let lx = (origin.X - radius) - e
        let ly = (origin.Y - radius) - e
        let lz = (origin.Z - radius) - e

        let hx = (origin.X + radius) + e
        let hy = (origin.Y + radius) + e
        let hz = (origin.Z + radius) + e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    member this.NormalAtPoint (p:Point) = 
        (p - origin).Normalise

    member this.getTextureCoords (p:Point) = 
        let n = (this.NormalAtPoint p)
        let theta = Math.Acos n.Y
        let phiNot = Math.Atan2(n.X, n.Z)
        let phi = if phiNot < 0. then (phiNot + 2.)*Math.PI else phiNot
        let u = phi / (2. * Math.PI)
        let v = 1.0-(theta / Math.PI)
        (u, v)

    member this.GetDiscriminant (ray:Ray) = 
        let s = (ray.GetOrigin - origin)
        let rayDir = ray.GetDirection.Normalise
        let sv = s * rayDir
        let ss = s * s
        sv*sv - ss + radius * radius

    override this.hitFunction (r:Ray) = 
        let D = this.GetDiscriminant r
        if D < 0. then HitPoint(r)
        else
            let s = (r.GetOrigin - origin)
            let rayDir = r.GetDirection.Normalise
            let sv = s * rayDir
            let ss = s * s
            let (t1,t2) = (-sv + Math.Sqrt(D), -sv - Math.Sqrt(D))
            if t1 < 0. && t2 < 0. then HitPoint(r)
            else if t1 < t2 then HitPoint(r, t1, this.NormalAtPoint (r.PointAtTime t1), tex) //this.getTextureCoords (r.PointAtTime t1))
            else HitPoint(r, t2, this.NormalAtPoint (r.PointAtTime t2), tex) //this.getTextureCoords (r.PointAtTime t2))



type HollowCylinder(center:Point, radius:float, height:float, tex:Material) = //change back to texture
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    member this.tex = tex

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...
    override this.getBoundingBox () = 
        let e = 0.000001
        let lx = (center.X - radius) - e
        let ly = (center.Y - (height/2.)) - e //height instead of radius for the Y coord
        let lz = (center.Z - radius) - e

        let hx = (center.X + radius) + e
        let hy = (center.Y + (height/2.)) + e //height instead of radius for the Y coord
        let hz = (center.Z + radius) + e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    member this.NormalAtPoint (p:Point):Vector =
        new Vector(p.X/radius, 0.0, p.Z/radius)

    member this.getTextureCoords (p:Point) = 
        let n = (this.NormalAtPoint p)
        let phiNot = Math.Atan2(n.X, n.Z)
        let phi = if phiNot < 0. then (phiNot + 2.)*Math.PI else phiNot
        let u = phi / (2. * Math.PI)
        let v = (p.Y / height) + (1. / 2.)
        (u, v)

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
        |(0.0) -> match (t1,t2) with //if D = 0 then t1 = t2, clean code...
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

    override this.isInside (p:Point) = 
        if (p.X**2. + p.Z**2.) <= radius**2. then //checks if the point lies within the bounds of the cylinders radius (similar to checking for discs)
            if -(height/2.) <= p.Y && p.Y <= (height/2.) then true //checks if the point lies between the 2 discs, so not above or below the cylinder
            else false
        else false
    override this.getBoundingBox () = 
        let e = 0.000001
        let lx = (center.X - radius) - e
        let ly = (center.Y - (height/2.)) - e //height instead of radius for the Y coord
        let lz = (center.Z - radius) - e

        let hx = (center.X + radius) + e
        let hy = (center.Y + (height/2.)) + e //height instead of radius for the Y coord
        let hz = (center.Z + radius) + e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    member this.getTextureCoords (p:Point) = //NotImplementedException()
                                               (0.0, 0.0)

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

    override this.isInside (p:Point) =
        if low.X <= p.X && p.X <= high.X then
            if low.Y <= p.Y && p.Y <= high.Y then
                if low.Z <= p.Z && p.Z <= high.Z then true
                else false
            else false
        else false

    override this.getBoundingBox () = 
        let e = 0.000001
        BBox(Point(low.X-e, low.Y-e, low.Z-e), Point(high.X+e, high.Y+e, high.Z+e))

    member this.getTextureCoords (p:Point) =  //this is likely wrong, the lecture notes are not detailed about how i should construct this (p. 37)
        let width = high.X - low.X
        let height = high.Y - low.Y
        ((p.X / width), (p.Y / height))

    override this.hitFunction (r:Ray) = 
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
                                                                else HitPoint(r, t, Vector(-1.0, 0.0, 0.0), left)
                |(tx',ty',tz') when ty' <= tx' && ty' <= tz' -> if r.GetDirection.Y > 0.0 then HitPoint(r, t, Vector(0.0, 1.0, 0.0), top) //when ty' is the smallest and t > 0.0
                                                                else HitPoint(r, t, Vector(0.0, -1.0, 0.0), bottom)
                |(tx',ty',tz') when tz' <= tx' && tz' <= ty' -> if r.GetDirection.Z > 0.0 then HitPoint(r, t, Vector(0.0, 0.0, 1.0), front) //when tz' is the smallest and t > 0.0
                                                                else HitPoint(r, t, Vector(0.0, 0.0, -1.0), back)
        else HitPoint(r)
        

type InfinitePlane(tex:Material) = 
    inherit Shape()
    member this.tex = tex
    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...
    override this.getBoundingBox () = failwith "Cannot make Bounding Box for infinite Plane"
    //override this.getTextureCoords (p:Point) = ((p.X), (p.Y))
    override this.hitFunction (r:Ray) = 
        let t = -(r.GetOrigin.Z / r.GetDirection.Z)
        if r.GetDirection.Z <> 0.0 && t > 0.0 then HitPoint(r, t, Vector(0.0, 0.0, 1.0), tex) else HitPoint(r)

type TransformShape (hitFunction) =
    inherit Shape()
    override this.isInside (p:Point) = failwith "Unsure what to do with TansformShape"
    override this.getBoundingBox () = failwith "Unsure what to do with TansformShape"
    default this.hitFunction(r: Ray) = hitFunction r


type CSGOperator = Union | Intersection | Subtraction | Grouping

type CSG(s1:Shape, s2:Shape, op:CSGOperator) =
    inherit Shape()
    member this.s1 = s1
    member this.s2 = s2
    member this.op = op
    override this.isInside (p:Point) = match op with
                                        |Union -> if s1.isInside p || s2.isInside p then true
                                                  else false
                                        |Intersection -> if s1.isInside p && s2.isInside p then true
                                                         else false
                                        |Subtraction -> failwith "not implemented yet"
                                        |Grouping -> failwith "not implemented yet"

    override this.getBoundingBox () = failwith "not implemented yet"

    //Union
    member this.unionHitFunctionInside (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r
        let s1Time = if s1Hit.DidHit then s1Hit.Time else infinity
        let s2Time = if s2Hit.DidHit then s2Hit.Time else infinity

        //compare the two times, and continue to work with the closest one (shouldnt be possible for both to miss)
        if s1Time <= s2Time then if s2.isInside (r.PointAtTime s1Time) then 
                                    this.unionHitFunctionInside (new Ray((r.PointAtTime s1Time), r.GetDirection))//keep firing the ray (might have to move the origin forward a bit
                                 else s1Hit //if the hit, is not inside s2, we have found the hitpoint
        else if s1.isInside (r.PointAtTime s2Time) then 
                this.unionHitFunctionInside (new Ray((r.PointAtTime s2Time), r.GetDirection))//keep firing the ray (might have to move the origin forward a bit
             else s2Hit //if the hit, is not inside s1, we have found the hitpoint

    member this.unionHitFunction (r:Ray) = match this.isInside r.GetOrigin with
                                           |false -> 
                                                let s1Hit = s1.hitFunction r
                                                let s2Hit = s2.hitFunction r
                                                let s1Time = if s1Hit.DidHit then s1Hit.Time else infinity
                                                let s2Time = if s2Hit.DidHit then s2Hit.Time else infinity
                                                if s1Time <= s2Time then s1Hit else s2Hit
                                            |true -> this.unionHitFunctionInside r
                                               
    //Intersection
    member this.intersectionHitFunction (r:Ray) = 
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r
        let s1Time = if s1Hit.DidHit then s1Hit.Time else infinity
        let s2Time = if s2Hit.DidHit then s2Hit.Time else infinity
        
        //check if the hitpoint for the smallest time, is inside the other shape, if it is, return that hitpoint, else fire a new ray
        if s1Time <= s2Time then if s2.isInside (r.PointAtTime s1Time) then s1Hit
                                 else this.intersectionHitFunction (new Ray((r.PointAtTime s1Time), r.GetDirection)) //fire new ray, (might have to move point furthe forward)
        else if s1.isInside (r.PointAtTime s2Time) then s1Hit
             else this.intersectionHitFunction (new Ray((r.PointAtTime s2Time), r.GetDirection)) //fire new ray, (might have to move point furthe forward)


    //Seperation


    //Grouping



    override this.hitFunction (r:Ray) = match op with
                                        |Union -> this.unionHitFunction r
                                        |Intersection -> this.intersectionHitFunction r
                                        |Subtraction -> failwith "not implemented yet"
                                        |Grouping -> failwith "not implemented yet"