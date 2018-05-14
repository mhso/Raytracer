namespace Tracer.Basics
open System
open Transformation

//exception BoundingBoxException

///////////////////////////////////
/////////////SHAPES!!!/////////////
///////////////////////////////////

////RECTANGLE////
type Rectangle(bottomLeft:Point, topLeft:Point, bottomRight:Point, tex:Texture)=
    inherit Shape()
    member this.bottomLeft = bottomLeft
    member this.topLeft = topLeft
    member this.bottomRight = bottomRight
    member this.tex = tex
    member this.width = bottomRight.X - bottomLeft.X
    member this.height = topLeft.Y - bottomLeft.Y
    member this.normal:Vector = new Vector(0.0, 0.0, 1.0)
    member this.bBox = 
        let e = 0.000001
        let lx = (min bottomLeft.X (min topLeft.X bottomRight.X)) - e
        let ly = (min bottomLeft.Y (min topLeft.Y bottomRight.Y)) - e
        let lz = - e

        let hx = (max bottomLeft.X (max topLeft.X bottomRight.X)) + e
        let hy = (max bottomLeft.Y (max topLeft.Y bottomRight.Y)) + e
        let hz = e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...

    override this.getBoundingBox () = this.bBox

    override this.hitFunction (r:Ray) = 
        match r with
        |(r) when (r.GetDirection.Z) = 0.0 -> HitPoint(r) //This method checks if dz = 0.0, which would make the ray, parrallel to the plane 
        |(r) when (-((r.GetOrigin.Z) / (r.GetDirection.Z))) <= 0.0 -> HitPoint(r) //This checks if t is 0 or smaller, in which case there is no hit
        |(r) -> let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z)))
                let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                if (px > 0.0 && px < this.width) && (py > 0.0 && py < this.height) then 
                    let func = Textures.getFunc tex
                    let u = (px / this.width)
                    let v = (py / this.height)
                    let mat = func u v
                    HitPoint(r, t, this.normal, mat, this, u, v) 
                else HitPoint(r)

                      
////DISC////
type Disc(center:Point, radius:float, tex:Texture)=
    inherit Shape()
    member this.center = center
    member this.radius = Math.Abs radius // must not be a negative number
    member this.tex = tex
    member this.bBox = 
        let e = 0.000001
        let lx = -radius - e
        let ly = -radius - e
        let lz = -e

        let hx = radius + e
        let hy = radius + e
        let hz = e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...

    override this.getBoundingBox () = this.bBox

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
                            let func = Textures.getFunc tex
                            let u = (px + radius)/(2.*radius)
                            let v = (py + radius)/(2.*radius)
                            let mat = func u v
                            HitPoint(r, t, this.normal, mat, this, u, v) 
                    else HitPoint(r)


////TRIANGLE////
and Triangle(a:Point, b:Point, c:Point, mat:Material)=
    inherit Shape()
    let mutable be : float = 0.0
    let mutable ga : float = 0.0
    member this.a = a
    member this.b = b
    member this.c = c
    member this.mat = mat
    member this.u = a-b //in case of errors try swithing a and b around
    member this.v = a-c // same here

    member this.n = this.u.CrossProduct this.v

    //the many members are for simplifying cramers rule and hit function
    member this.pa = ((a.X)-(b.X))
    member this.pb = ((a.X)-(c.X))
    member this.e = ((a.Y)-(b.Y))
    member this.f = ((a.Y)-(c.Y))
    member this.i = ((a.Z)-(b.Z))
    member this.j = ((a.Z)-(c.Z))

    member this.bBox =
        let e = 0.000001
        let lx = (min a.X (min b.X c.X)) - e
        let ly = (min a.Y (min b.Y c.Y)) - e
        let lz = (min a.Z (min b.Z c.Z)) - e 

        let hx = (max a.X (max b.X c.X)) + e
        let hy = (max a.Y (max b.Y c.Y)) + e
        let hz = (max a.Z (max b.Z c.Z)) + e 

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))
        
    member this.beta with get() = be and set(value) = be <- value
    member this.gamma with get() = ga and set(value) = ga <- value

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...

    override this.getBoundingBox () = this.bBox
    
    //the many let statements are for simplifying cramers rule
    override this.hitFunction (r:Ray) = 
        let pc = (r.GetDirection.X)
        let g = (r.GetDirection.Y)
        let k = (r.GetDirection.Z)


        match r with
            |(r) when (this.pa*((this.f*(k))-((g)*this.j)) + this.pb*(((g)*this.i)-(this.e*(k))) + (pc)*((this.e*this.j)-(this.f*this.i))) = 0.0 -> HitPoint(r)
            |(r) -> let d = ((a.X)-(r.GetOrigin.X)) 
                    let h = ((a.Y)-(r.GetOrigin.Y)) 
                    let l = ((a.Z)-(r.GetOrigin.Z))
                    let D = (this.pa*((this.f*k)-(g*this.j)) + this.pb*((g*this.i)-(this.e*k)) + pc*((this.e*this.j)-(this.f*this.i)))
                    let x = (d*((this.f*k)-(g*this.j)) + this.pb*((g*l)-(h*k)) + pc*((h*this.j)-(this.f*l)))/D
                    let y = (this.pa*((h*k)-(g*l)) + d*((g*this.i)-(this.e*k)) + pc*((this.e*l)-(h*this.i)))/D
                    let z = (this.pa*((this.f*l)-(h*this.j)) + this.pb*((h*this.i)-(this.e*l)) + d*((this.e*this.j)-(this.f*this.i)))/D
                    //x=beta, y=gamma, z=t
                    //alpha is gained from 1-x-y, this is used for texturing (alpha, beta, gamma that is)
                    if (x <= 1.0 && x >= 0.0) && (y <= 1.0 && y >= 0.0) && (x+y <= 1.0 && x+y >= 0.0) && (z>0.0) then
                            this.beta <- x
                            this.gamma <- y
                            HitPoint(r, z, (this.u % this.v).Normalise, mat, this) else HitPoint(r) //why mat instead of texture???
                            


////SPHERE////
type SphereShape(origin: Point, radius: float, tex: Texture) = 
    inherit Shape()

    let pimult2 = 2. * Math.PI

    member this.origin = origin 
    member this.radius = radius
    member this.tex = tex
    member this.bBox = //no point on the sphere should be larger than the center point + the radius.
        let e = 0.000001
        let lx = - radius - e //as the sphere is alwas spawned in 0,0,0´the bounding box should not take the origin value into consideration
        let ly = - radius - e
        let lz = - radius - e

        let hx = radius + e
        let hy = radius + e
        let hz = radius + e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    override this.isInside (p:Point) =
        let x = (p.X - origin.X)**2. + (p.Y - origin.Y)**2. + (p.Z - origin.Z)**2. // i might be able to remove origin from this, as it should always be 0,0,0
        (x < (radius**2.))

    override this.getBoundingBox () = this.bBox    

    member this.NormalAtPoint (p:Point) = 
        (p - origin).Normalise //can i remove origin from here as well???
    
    member this.getTextureCoords (p:Point) =
        let n = this.NormalAtPoint p
        (Math.Atan2(n.X, n.Z) / pimult2, 1. - (Math.Acos(n.Y) / Math.PI))

    member this.determineHitPoint (r:Ray) (t:float) = 
        let p = r.PointAtTime t
        let uv = this.getTextureCoords (r.PointAtTime t)
        let u = fst uv
        let v = snd uv
        let func = Textures.getFunc tex
        let mat = func u v 
        HitPoint(r, t, p.ToVector.Normalise, mat, this, u, v)

    override this.hitFunction (r:Ray) = 
        match (this.bBox.intersect r).IsSome with
        |true ->
            let a = (r.GetDirection.X**2.) + (r.GetDirection.Y**2.) + (r.GetDirection.Z**2.) //Determines a in the quadratic equation
            let b = 2. * ((r.GetOrigin.X * r.GetDirection.X) + (r.GetOrigin.Y * r.GetDirection.Y) + (r.GetOrigin.Z * r.GetDirection.Z))//Determines b in the quadratic equation
            let c = (r.GetOrigin.X**2.) + (r.GetOrigin.Y**2.) + (r.GetOrigin.Z**2.) - (radius**2.) //Determines c in the quadratic equation
            let D = (b**2.)-4.*a*c 
            let t1 = (-b + Math.Sqrt(D))/(2.0 * a)
            let t2 = (-b - Math.Sqrt(D))/(2.0 * a)
            match D with
            |(0.0) -> match t1 > 0.0 with
                      |true -> this.determineHitPoint r t1 
                      |false -> HitPoint(r)
            |(D) when D < 0.0 -> HitPoint(r)
            |(D) -> match (t1,t2) with //when D > 0.0, and there are two valid values for t
                      |(t1,t2) when t1 <= 0.0 && t2 <= 0.0 -> HitPoint(r)
                      |(t1,t2) -> match (t1 < t2 && t1 > 0.0) with
                                  |true -> this.determineHitPoint r t1 
                                  |false when t2 > 0.0 -> this.determineHitPoint r t2    
                                  |false -> this.determineHitPoint r t1
        |false -> HitPoint(r)


////HOLLOWCYLINDER////
type HollowCylinder(center:Point, radius:float, height:float, tex:Texture) = //change back to texture
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    member this.tex = tex
    member this.bBox = 
        let e = 0.000001
        let lx = - radius - e
        let ly = - (height/2.) - e //height instead of radius for the Y coord
        let lz = - radius - e

        let hx = radius + e
        let hy = (height/2.) + e //height instead of radius for the Y coord
        let hz = radius + e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...

    override this.getBoundingBox () = this.bBox

    member this.NormalAtPoint (p:Point):Vector =
        new Vector(p.X/radius, 0.0, p.Z/radius)
    
    member this.getTextureCoords (p:Point) =
        let n = (this.NormalAtPoint p)
        let phiNot = Math.Atan2(n.X, n.Z)
        let phi = match phiNot < 0. with
                  |true -> (phiNot + 2.)*Math.PI 
                  |false -> phiNot
        let u = phi / (2. * Math.PI)
        let v = (p.Y / height) + (1. / 2.)
        (u, v)

    member this.determineHitPoint (r:Ray) (t:float) = 
        let p = r.PointAtTime t
        let uv = this.getTextureCoords (r.PointAtTime t)
        let u = fst uv
        let v = snd uv
        let func = Textures.getFunc tex
        let mat = func u v 
        HitPoint(r, t, Vector(p.X/radius, 0.0, p.Z/radius), mat, this, u, v) 

    member this.determineIfPointIsInsideHeight (r:Ray) (t:float) =
        let p = r.PointAtTime t
        (p.Y > -(height/2.0) && p.Y < (height/2.0))

    override this.hitFunction (r:Ray) = 
        match (this.bBox.intersect r).IsSome with
        |true ->
            let a = ((r.GetDirection.X)**2.0) + ((r.GetDirection.Z)**2.0) //both are to the power of 2
            let b = 2.0*((r.GetOrigin.X * r.GetDirection.X)+(r.GetOrigin.Z * r.GetDirection.Z))
            let c = ((r.GetOrigin.X)**2.0) + ((r.GetOrigin.Z)**2.0) - (radius**2.0)
            let D = (b**2.0) - 4.0*a*c
            let t1 = (-b + Math.Sqrt(D))/(2.0 * a)
            let t2 = (-b - Math.Sqrt(D))/(2.0 * a)
            match D with
            |(0.0) -> match t1 <= 0.0 with //if D=0 then t1 = t2
                      |true -> HitPoint(r) 
                      |false -> let p = r.PointAtTime t1
                                match (p.Y > -(height/2.0) && p.Y < (height/2.0)) with
                                |true -> this.determineHitPoint r t1 
                                |false -> HitPoint(r)
            |(D) when D < 0.0 -> HitPoint(r)
            |(_) -> match (t1,t2) with //when D > 0.0, and there are two valid values for t
                    |(t1,t2) when t1 <= 0.0 && t2 <= 0.0 -> HitPoint(r) //if both t's are a miss
                    |(t1,t2) -> match t2 < t1 && t2 > 0.0 with
                                |true ->
                                    match this.determineIfPointIsInsideHeight r t2 with
                                    |true -> this.determineHitPoint r t2 
                                    |false -> match this.determineIfPointIsInsideHeight r t1 with
                                              |true -> this.determineHitPoint r t1
                                              |false -> HitPoint(r)
                                |false -> match t1 > 0.0 with
                                          |true ->  match this.determineIfPointIsInsideHeight r t1 with
                                                    |true -> this.determineHitPoint r t1
                                                    |false -> HitPoint(r)
                                          |false -> match this.determineIfPointIsInsideHeight r t2 with
                                                    |true -> this.determineHitPoint r t2
                                                    |false -> HitPoint(r)                        
                        
                        
                                (*if t2 < t1 && t2 > 0.0 then
                                    let p2 = r.PointAtTime t2
                                    if p2.Y > -(height/2.0) && p2.Y < (height/2.0) then this.determineHitPoint r t2 
                                    else let p1 = r.PointAtTime t1
                                         if p1.Y > -(height/2.0) && p1.Y < (height/2.0) then this.determineHitPoint r t1
                                         else HitPoint(r)
                                else if t1 > 0.0 then
                                         let p1 = r.PointAtTime t1
                                         if p1.Y > -(height/2.0) && p1.Y < (height/2.0) then this.determineHitPoint r t1
                                         else HitPoint(r)
                                     else 
                                         let p2 = r.PointAtTime t2
                                         if p2.Y > -(height/2.0) && p2.Y < (height/2.0) then this.determineHitPoint r t2
                                         else HitPoint(r)*)
                                         
        |false -> HitPoint(r)

////TRANSFORM////                                                                                     
module Transform =
    let transformRay (r : Ray) t = 
        let o = pointToMatrix r.GetOrigin
        let d = vectorToMatrix r.GetDirection
        let invT = getInvMatrix t
        let originMatrix = Matrix.multi (invT, o)
        let directionMatrix = Matrix.multi (invT, d)
        let origin = matrixToPoint originMatrix
        let direction = matrixToVector directionMatrix
        new Ray(origin, direction)

    let transformNormal (v:Vector) (t: Transformation.Transformation)= 
        let tVector = matrixToVector (Matrix.multi ((transpose (getInvMatrix (t))),(vectorToMatrix v)))
        tVector

    let transform (s : Shape) (t:Transformation) =
        {new Shape() with
            member this.hitFunction r = 
                let transformedRay = transformRay r t
                let hitsOriginal = s.hitFunction transformedRay
                if (hitsOriginal.DidHit) then
                    let normal = transformNormal (hitsOriginal.Normal) t
                    new HitPoint(r, hitsOriginal.Time, normal, hitsOriginal.Material, hitsOriginal.Shape, hitsOriginal.U, hitsOriginal.V)
                else 
                    new HitPoint(r)
            member this.getBoundingBox () = 
                let bbH = s.getBoundingBox().highPoint
                let bbL = s.getBoundingBox().lowPoint
                let vertex = 
                    [|bbH; 
                    Point(bbL.X, bbH.Y, bbH.Z);
                    Point(bbL.X, bbH.Y, bbL.Z);
                    Point(bbH.X, bbH.Y, bbL.Z);
                    Point(bbH.X, bbL.Y, bbH.Z);
                    Point(bbL.X, bbL.Y, bbH.Z);
                    bbL;
                    Point(bbH.X, bbL.Y, bbL.Z)|]
                let newPos = Array.zeroCreate(2)
                let firstPoint = matrixToPoint (Matrix.multi ((getMatrix t),pointToMatrix vertex.[0]))
                newPos.[0] <- firstPoint
                newPos.[1] <- firstPoint
                for i in 1..7 do 
                    let newPoint = matrixToPoint (Matrix.multi ((getMatrix t),pointToMatrix vertex.[i]))
                    newPos.[0] <- (newPos.[0]).Lowest newPoint
                    let pos1 = newPos.[1]
                    newPos.[1] <- pos1.Highest newPoint
                BBox(newPos.[0],newPos.[1])
            member this.isInside p = 
                let oldP = matrixToPoint (Matrix.multi(getInvMatrix t, pointToMatrix p))
                s.isInside(oldP)
        }
        

////SOLIDCYLINDER////
type SolidCylinder(center:Point, radius:float, height:float, cylinder:Texture, top:Texture, bottom:Texture) =
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    member this.cylinder = cylinder
    member this.top = top
    member this.bottom = bottom
    //builds the transformed discs at the top and bottom of the solid cylinder
    member this.topDisc = 
        let rotate = rotateX (Math.PI/2.)
        let move = translate 0. (height/2.) 0.
        let mergeTrans = mergeTransformations [rotate; move]
        Transform.transform (Disc(Point(0.,0.,0.), radius, top)) mergeTrans
    member this.bottomDisc = 
        let rotate = rotateX (Math.PI/2.)
        let move = translate 0. -(height/2.) 0.
        let mergeTrans = mergeTransformations [rotate; move]
        Transform.transform (Disc(Point(0.,0.,0.), radius, bottom)) mergeTrans
    //builds the hollow cylinder
    member this.hollowCylinder = HollowCylinder(center, radius, height, cylinder)
    member this.bBox = this.hollowCylinder.bBox
        //should only need the BBox of the Hollow Cylinder

        (*
        let e = 0.000001
        let lx = (center.X - radius) - e
        let ly = (center.Y - (height/2.)) - e //height instead of radius for the Y coord
        let lz = (center.Z - radius) - e

        let hx = (center.X + radius) + e
        let hy = (center.Y + (height/2.)) + e //height instead of radius for the Y coord
        let hz = (center.Z + radius) + e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))
        *)

    override this.isInside (p:Point) = 
        match ((p.X**2. + p.Z**2.) <= radius**2.) with //checks if the point lies within the bounds of the cylinders radius (similar to checking for discs)
        |true -> (-(height/2.) <= p.Y && p.Y <= (height/2.)) //checks if the point lies between the 2 discs, so not above or below the cylinder
        |false -> false

    override this.getBoundingBox () = this.bBox

    override this.hitFunction (r:Ray) = 
        match (this.bBox.intersect(r)).IsSome with
        |true -> 
            // look for hitPoints
            let hpTop = this.topDisc.hitFunction r
            let hpBottom = this.bottomDisc.hitFunction r
            let hpCylinder = this.hollowCylinder.hitFunction r
            //extract time from hitPoints
            let tTop = match hpTop.DidHit with
                            |true -> hpTop.Time
                            |false -> infinity
            let tBottom = match hpBottom.DidHit with
                            |true -> hpBottom.Time
                            |false -> infinity
            let tCylinder = match hpCylinder.DidHit with
                            |true -> hpCylinder.Time
                            |false -> infinity

            //Compare t values
            match (tTop, tBottom, tCylinder) with
            |(top, bottom, cylinder) when top = bottom && bottom = cylinder -> HitPoint(r)
            |(top, bottom, cylinder) when cylinder < bottom && cylinder < top ->  hpCylinder
            |(top, bottom, cylinder) when top < bottom && top < cylinder ->  hpTop
            |(top, bottom, cylinder) when bottom < top && bottom < cylinder ->  hpBottom
            |(_,_,_) -> HitPoint(r)
        |false -> HitPoint(r)


////BOX////
type Box(low:Point, high:Point, front:Texture, back:Texture, top:Texture, bottom:Texture, left:Texture, right:Texture) = 
    inherit Shape()
    member this.low = low
    member this.high = high
    member this.front = front
    member this.back = back
    member this.top = top
    member this.bottom = bottom
    member this.left = left
    member this.right = right
    member this.width = high.X - low.X
    member this.height = high.Y - low.Y
    member this.depth = high.Z - low.Z

    override this.isInside (p:Point) =
        match (low.X <= p.X && p.X <= high.X) with
        |true ->
            match (low.Y <= p.Y && p.Y <= high.Y) with
            |true -> (low.Z <= p.Z && p.Z <= high.Z) 
            |false -> false
        |false -> false

    override this.getBoundingBox () = 
        let e = 0.000001
        BBox(Point(low.X-e, low.Y-e, low.Z-e), Point(high.X+e, high.Y+e, high.Z+e))
    
    member this.getMatFromTex (tex:Texture) (u:float) (v:float) =
        let func = Textures.getFunc tex
        let mat = func u v 
        mat

    override this.hitFunction (r:Ray) = 

        let boolX = r.GetDirection.X >= 0.0
        let boolY = r.GetDirection.Y >= 0.0
        let boolZ = r.GetDirection.Z >= 0.0
        
        let tx =  match boolX with
                  |true -> (low.X - r.GetOrigin.X)/r.GetDirection.X 
                  |false -> (high.X - r.GetOrigin.X)/r.GetDirection.X
        let tx' = match boolX with
                  |true -> (high.X - r.GetOrigin.X)/r.GetDirection.X 
                  |false -> (low.X - r.GetOrigin.X)/r.GetDirection.X
        let ty =  match boolY with
                  |true -> (low.Y - r.GetOrigin.Y)/r.GetDirection.Y 
                  |false -> (high.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let ty' = match boolY with
                  |true -> (high.Y - r.GetOrigin.Y)/r.GetDirection.Y 
                  |false -> (low.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let tz =  match boolZ with
                  |true -> (low.Z - r.GetOrigin.Z)/r.GetDirection.Z 
                  |false -> (high.Z - r.GetOrigin.Z)/r.GetDirection.Z
        let tz' = match boolZ with
                  |true -> (high.Z - r.GetOrigin.Z)/r.GetDirection.Z 
                  |false -> (low.Z - r.GetOrigin.Z)/r.GetDirection.Z
        
        let t = max tx (max ty tz)

        let t' = min tx' (min ty' tz')

        let d = false //someones debug stuff???

        match (t < t' && t' > 0.0) with
        |true ->
            match t > 0.0 with
            |true -> 
                match (tx, ty, tz) with
                |(tx,ty,tz) when tx >= ty && tx >= tz -> match r.GetDirection.X > 0.0 with
                                                         |true -> 
                                                            if d then printfn "1"
                                                            let u = (r.PointAtTime(t).Y - low.Y) / this.height
                                                            let v = (r.PointAtTime(t).Z - low.Z) / this.depth
                                                            HitPoint(r, t, Vector(-1.0, 0.0, 0.0), (this.getMatFromTex left u v), this, u, v) //when tx is the biggest and t > 0.0
                                                         |false -> 
                                                            if d then printfn "2"
                                                            let u = (r.PointAtTime(t).Y - low.Y) / this.height
                                                            let v = (r.PointAtTime(t).Z - low.Z) / this.depth
                                                            HitPoint(r, t, Vector(1.0,0.0,0.0), (this.getMatFromTex right u v), this, u, v)
                |(tx,ty,tz) when ty >= tx && ty >= tz -> match r.GetDirection.Y > 0.0 with
                                                         |true ->
                                                            if d then printfn "3"
                                                            let u = (r.PointAtTime(t).X - low.X) / this.width
                                                            let v = (r.PointAtTime(t).Z - low.Z) / this.depth
                                                            HitPoint(r, t, Vector(0.0, -1.0, 0.0), (this.getMatFromTex bottom u v), this, u, v) //when ty is the biggest and t > 0.0
                                                         |false -> 
                                                            if d then printfn "4"
                                                            let u = (r.PointAtTime(t).X - low.X) / this.width
                                                            let v = (r.PointAtTime(t).Z - low.Z) / this.depth
                                                            HitPoint(r, t, Vector(0.0, 1.0, 0.0), (this.getMatFromTex top u v), this, u, v)
                |(tx,ty,tz) when tz >= tx && tz >= ty -> match r.GetDirection.Z > 0.0 with
                                                         |true ->
                                                            if d then printfn "5"
                                                            let u = (r.PointAtTime(t).X - low.X) / this.width
                                                            let v = (r.PointAtTime(t).Y - low.Y) / this.height
                                                            HitPoint(r, t, Vector(0.0, 0.0, -1.0), (this.getMatFromTex back u v), this, u, v) //when tz is the biggest and t > 0.0
                                                         |false ->
                                                            if d then printfn "6"
                                                            let u = (r.PointAtTime(t).X - low.X) / this.width
                                                            let v = (r.PointAtTime(t).Y - low.Y) / this.height
                                                            HitPoint(r, t, Vector(0.0, 0.0, 1.0), (this.getMatFromTex front u v), this, u, v)
                |(_,_,_) -> failwith "shouldn't reach this point"
            |false ->
                match (tx', ty', tz') with
                |(tx',ty',tz') when tx' <= ty' && tx' <= tz' -> match r.GetDirection.X > 0.0 with
                                                                |true ->
                                                                    if d then printfn "7"
                                                                    let u = (r.PointAtTime(t').Y - low.Y) / this.height
                                                                    let v = (r.PointAtTime(t').Z - low.Z) / this.depth
                                                                    HitPoint(r, t', Vector(-1.0, 0.0, 0.0), (this.getMatFromTex right u v), this, u, v) //when tx' is the smallest and t > 0.0
                                                                |false ->
                                                                    if d then printfn "8"
                                                                    let u = (r.PointAtTime(t').Y - low.Y) / this.height
                                                                    let v = (r.PointAtTime(t').Z - low.Z) / this.depth
                                                                    HitPoint(r, t', Vector(1.0, 0.0, 0.0), (this.getMatFromTex left u v), this, u, v)
                |(tx',ty',tz') when ty' <= tx' && ty' <= tz' -> match r.GetDirection.Y > 0.0 with
                                                                |true ->
                                                                    if d then printfn "9"
                                                                    let u = (r.PointAtTime(t').X - low.X) / this.width
                                                                    let v = (r.PointAtTime(t').Z - low.Z) / this.depth
                                                                    HitPoint(r, t', Vector(0.0, -1.0, 0.0), (this.getMatFromTex top u v), this, u, v) //when ty' is the smallest and t > 0.0
                                                                |false ->
                                                                    if d then printfn "10"
                                                                    let u = (r.PointAtTime(t').X - low.X) / this.width
                                                                    let v = (r.PointAtTime(t').Z - low.Z) / this.depth
                                                                    HitPoint(r, t', Vector(0.0, 1.0, 0.0), (this.getMatFromTex bottom u v), this, u, v)
                |(tx',ty',tz') when tz' <= tx' && tz' <= ty' -> match r.GetDirection.Z > 0.0 with
                                                                |true ->
                                                                    if d then printfn "11"
                                                                    let u = (r.PointAtTime(t').X - low.X) / this.width
                                                                    let v = (r.PointAtTime(t').Y - low.Y) / this.height
                                                                    HitPoint(r, t', Vector(0.0, 0.0, -1.0), (this.getMatFromTex front u v), this, u, v) //when tz' is the smallest and t > 0.0
                                                                |false ->
                                                                    if d then printfn "12"
                                                                    //printfn "width: %A, height: %A, depth: %A" this.width this.height this.depth
                                                                    let u = (r.PointAtTime(t').X - low.X) / this.width
                                                                    let v = (r.PointAtTime(t').Y - low.Y) / this.height 
                                                                    HitPoint(r, t', Vector(0.0, 0.0, 1.0), (this.getMatFromTex back u v), this, u, v)
                |(_,_,_) -> failwith "shouldn't reach this point"
        |false -> HitPoint(r)
        

////INFINITEPLANE////
type InfinitePlane(tex:Texture) = 
    inherit Shape()
    member this.tex = tex
    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" //this could also just return false...
    override this.getBoundingBox () = failwith "Infinite Plane cannot have a Bounding Box"
    override this.hitFunction (r:Ray) = 
        let t = -(r.GetOrigin.Z / r.GetDirection.Z) //the plane is on the x-z plane, as this fits with the coordinate system, we have been asked to use.
        match (r.GetDirection.Z <> 0.0 && t > 0.0) with
        |true ->
            let func = Textures.getFunc tex
            let u = (r.PointAtTime t).X
            let v = (r.PointAtTime t).Y
            let mat = func u v
            HitPoint(r, t, Vector(0.0, 0.0, -1.0), mat, this, u, v)
        |false -> HitPoint(r)






///////////////////////////////////
////CONSTRUCTIVE SOLID GEOMETRY////
///////////////////////////////////

type CSGOperator = Union | Intersection | Subtraction | Grouping

type CSG(s1:Shape, s2:Shape, op:CSGOperator) =
    inherit Shape()
    member this.s1 = s1
    member this.s2 = s2
    member this.op = op
    member this.epsilon = 0.000001
    member this.bBox = match op with
                       |Union|Grouping -> //merges the two BBoxes, by combining the highest high coords, and the lowest low coords, to form a new bounding box
                            let bBox1 = s1.getBoundingBox ()
                            let bBox2 = s2.getBoundingBox ()
                            let newLow = Point((min bBox1.lowPoint.X bBox2.lowPoint.X), (min bBox1.lowPoint.Y bBox2.lowPoint.Y), (min bBox1.lowPoint.Z bBox2.lowPoint.Z))
                            let newHigh = Point((max bBox1.highPoint.X bBox2.highPoint.X), (max bBox1.highPoint.Y bBox2.highPoint.Y), (max bBox1.highPoint.Z bBox2.highPoint.Z))
                            BBox(newLow, newHigh)
                       |Intersection -> //chooses the highest of the low point coords, and the lowest of the highpoint coords, to approximate the intersection
                            let bBox1 = s1.getBoundingBox ()
                            let bBox2 = s2.getBoundingBox ()
                            let newLow = Point((max bBox1.lowPoint.X bBox2.lowPoint.X), (max bBox1.lowPoint.Y bBox2.lowPoint.Y), (max bBox1.lowPoint.Z bBox2.lowPoint.Z))
                            let newHigh = Point((min bBox1.highPoint.X bBox2.highPoint.X), (min bBox1.highPoint.Y bBox2.highPoint.Y), (min bBox1.highPoint.Z bBox2.highPoint.Z))
                            BBox(newLow, newHigh)
                       |Subtraction -> s1.getBoundingBox () //just returns the bounding box for s1

    override this.isInside (p:Point) = match op with //if-then-else can be removed here... silly me...
                                        |Union|Grouping -> (s1.isInside p || s2.isInside p)
                                        |Intersection -> (s1.isInside p && s2.isInside p)
                                        |Subtraction -> (s1.isInside p && (not (s2.isInside p)))

    override this.getBoundingBox () = this.bBox
                                        

    ////UNION////
    member this.unionHitFunctionInside (originalRay:Ray) (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r
        let s1Time = match s1Hit.DidHit with
                     |true -> s1Hit.Time 
                     |false -> infinity
        let s2Time = match s2Hit.DidHit with
                     |true -> s2Hit.Time 
                     |false -> infinity

        //i continue no matter what 

        //compare the two times, and continue to work with the closest one (shouldnt be possible for both to miss)
        match (s1Time <= (s2Time + this.epsilon)) with 
        |true -> 
            match (s2.isInside (r.PointAtTime s1Time)) with
            |true ->  
                let newOrigin = (r.PointAtTime s1Time).Move (r.GetDirection.MultScalar (this.epsilon))
                this.unionHitFunctionInside originalRay (new Ray(newOrigin, r.GetDirection))//keep firing the ray (might have to move the origin forward a bit
            |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime(s1Hit.Time)), s1Hit.Normal, s1Hit.Material, this) //if the hit, is not inside s2, we have found the hitpoint
        |false -> 
            match (s1.isInside (r.PointAtTime s2Time)) with
            |true ->
                let newnewOrigin = (r.PointAtTime s2Time).Move (r.GetDirection.MultScalar (this.epsilon))
                this.unionHitFunctionInside originalRay (new Ray(newnewOrigin, r.GetDirection))//keep firing the ray (might have to move the origin forward a bit
            |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime(s2Hit.Time)), s2Hit.Normal, s2Hit.Material, this) //if the hit, is not inside s1, we have found the hitpoint

    member this.unionHitFunction (r:Ray) = match this.isInside r.GetOrigin with
                                           |false -> 
                                                let s1Hit = s1.hitFunction r
                                                let s2Hit = s2.hitFunction r
                                                match (not(s1Hit.DidHit) && not(s2Hit.DidHit)) with // this check reduced the render time of 10 tri-Unions (3 solid cylinders), from 41.5 to 28 sek!!!
                                                |true ->
                                                    let s1Time = match s1Hit.DidHit with
                                                                 |true -> s1Hit.Time 
                                                                 |false -> infinity
                                                    let s2Time = match s2Hit.DidHit with
                                                                 |true -> s2Hit.Time 
                                                                 |false -> infinity
                                                    match (s1Time <= (s2Time + this.epsilon)) with
                                                    |true -> HitPoint(r, s1Hit.Time, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                                    |false -> HitPoint(r, s2Hit.Time, s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                                                |false -> HitPoint(r)
                                           |true -> this.unionHitFunctionInside r r
                                               
    ////INTERSECTION////
    member this.intersectionHitFunction (originalRay:Ray) (r:Ray) = 
        match (this.bBox.intersect r).IsSome with //check bounding box for intersect
        |true ->
            let s1Hit = s1.hitFunction r //fire ray at both shapes
            let s2Hit = s2.hitFunction r
            match (not(s1Hit.DidHit) && not(s1Hit.DidHit)) with //check if any hit was found
            |false ->
                let s1Time = match s1Hit.DidHit with
                             |true -> s1Hit.Time
                             |false -> infinity
                let s2Time = match s2Hit.DidHit with
                             |true -> s2Hit.Time
                             |false -> infinity
        

                match (s1Time, s2Time) with
                |(s1T, s2T) when s1T = infinity && s2T = infinity -> HitPoint(r) //if the ray misses
                |(s1T, s2T) when s1T = infinity -> match (s1.isInside (r.PointAtTime s2T)) with
                                                   |true -> 
                                                        HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s2Hit.Time), s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                                                   |false -> 
                                                       let newOrigin = (r.PointAtTime s2T).Move (r.GetDirection.MultScalar (this.epsilon))
                                                       this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection))
                |(s1T, s2T) when s2T = infinity -> match (s2.isInside (r.PointAtTime s1T)) with
                                                   |true -> 
                                                        HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s1Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                                   |false -> 
                                                       let newOrigin = (r.PointAtTime s1T).Move (r.GetDirection.MultScalar (this.epsilon))
                                                       this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection))
                |(s1T, s2T) when (s2T - this.epsilon) < s1T && s1T < (s2T + this.epsilon) -> 
                    HitPoint(originalRay, s1Hit.Time, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                |(s1T, s2T) -> 
                            //hit function, that fires rays fom the furthest hit, instead of the closest, might provide speed increase for more complex csg
                            match (s1T > s2T) with
                            |true -> 
                                match (s2.isInside (r.PointAtTime s1T)) with //might be able to condense this with next match
                                |true ->  
                                    match (s1.isInside (r.PointAtTime s2T)) with
                                    |true -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s2Hit.Time), s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                                    |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s1Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                |false -> 
                                    match (s1.isInside (r.PointAtTime s2T)) with
                                    |true -> s2Hit
                                    |false ->
                                        let newOrigin = (r.PointAtTime s1T).Move (r.GetDirection.MultScalar (this.epsilon))
                                        this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection))
                            |false -> 
                                match (s1.isInside (r.PointAtTime s2T)) with //might be able to condense this with next if
                                |true ->
                                    match (s2.isInside (r.PointAtTime s1T)) with
                                    |true -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s1Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                    |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s2Hit.Time), s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                                |false ->
                                    match (s2.isInside (r.PointAtTime s1T)) with
                                    |true -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s2Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                    |false -> 
                                        let newOrigin = (r.PointAtTime s2T).Move (r.GetDirection.MultScalar (this.epsilon))
                                        this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection))
            |true -> HitPoint(r)
        |false -> HitPoint(r)
                    
                    
                    //old hit function, that fires new rays from the shortest time
                    (*
                    if s1T <= s2T then if s2.isInside (r.PointAtTime s1T) then s1Hit
                                       else 
                                       let moveVector = Vector(r.GetDirection.X/1000., r.GetDirection.Y/1000., r.GetDirection.Z/1000.)
                                       let newOrigin = (r.PointAtTime s1T).Move moveVector
                                       this.intersectionHitFunction (new Ray(newOrigin, r.GetDirection)) //fire new ray, (might have to move point furthe forward)
                    else if s1.isInside (r.PointAtTime s2T) then s2Hit
                         else 
                            let moveVector = Vector(r.GetDirection.X/1000., r.GetDirection.Y/1000., r.GetDirection.Z/1000.)
                            let newOrigin = (r.PointAtTime s2T).Move moveVector
                            this.intersectionHitFunction (new Ray(newOrigin, r.GetDirection)) //fire new ray, (might have to move point furthe forward)
                    *)

    ////SUBTRACTION////
    (*
    member this.subtractionHitFunction (r:Ray) =
        let s2Hit = s2.hitFunction r //fire ray at second shapes
        if s2Hit.DidHit then 
            if s1.isInside (r.PointAtTime (s2Hit.Time)) then s2Hit
            else 
                let moveVector = Vector(r.GetDirection.X/1000., r.GetDirection.Y/1000., r.GetDirection.Z/1000.)
                let newOrigin = (r.PointAtTime s2Hit.Time).Move moveVector
                this.subtractionHitFunction (new Ray(newOrigin, r.GetDirection))
        else HitPoint(r)
    
    

    member this.subtractionHitFunction (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at first shapes
        if s1Hit.DidHit then 
            if s2.isInside (r.PointAtTime (s1Hit.Time)) then //refire Ray
                let moveVector = Vector(r.GetDirection.X/1000., r.GetDirection.Y/1000., r.GetDirection.Z/1000.)
                let newOrigin = (r.PointAtTime s1Hit.Time).Move moveVector
                this.subtractionHitFunctionHelper (new Ray(newOrigin, r.GetDirection))
            else s1Hit
        else HitPoint(r)
        *)

    member this.subtractionHitFunction (originalRay:Ray) (r:Ray) =
        match (this.bBox.intersect r).IsSome with //doesnt seem to make a big dfference...
        |true ->
            let s1Hit = s1.hitFunction r //fire ray at first shape
   
            match s1Hit.DidHit with 
            |true -> 
                match ( s2.isInside (r.PointAtTime (s1Hit.Time))) with
                |true -> //refire Ray
                    let newOrigin = (r.PointAtTime s1Hit.Time).Move (r.GetDirection.MultScalar (this.epsilon))
                    let r2 = new Ray(newOrigin, r.GetDirection) //make new ray, so you dont repeat hits
                    let s2Hit = s2.hitFunction r2 //fire new ray at second shape

                    match s2Hit.DidHit with //can it even not hit s2, after i make a new ray with origin inside s2?
                    |true ->
                        match s1.isInside (r2.PointAtTime (s2Hit.Time)) with 
                        |true ->                                                                       
                            HitPoint(originalRay, originalRay.TimeAtPoint(r2.PointAtTime s2Hit.Time), s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                        |false -> 
                            let newnewOrigin = (r2.PointAtTime s2Hit.Time).Move (r2.GetDirection.MultScalar (this.epsilon))
                            this.subtractionHitFunction originalRay (new Ray(newnewOrigin, r2.GetDirection)) //the direction vector should be the same for r and r2
                    |false -> HitPoint(r)
                |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s1Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
            |false -> HitPoint(r)
        |false -> HitPoint(r)
    
    (*
    member this.subtractionHitFunction (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r
        let s1Time = if s1Hit.DidHit then s1Hit.Time else infinity
        let s2Time = if s2Hit.DidHit then s2Hit.Time else infinity

        match (s1Time, s2Time) with 
        |(s1T, s2T) when s1T = infinity && s2T = infinity -> HitPoint(r) //if the ray misses
        |(s1T, s2T) when s2T = infinity -> if s2.isInside then //refire 
                                           else s1Hit //hit on the s1 shape (the subtractee)
        |(s1T, s2T) when s1T = infinity -> if (s1.isInside (r.PointAtTime s2T)) && (not (s2.isInside (r.PointAtTime s2T))) then s2Hit
                                           else 
                                           let moveVector = Vector(r.GetDirection.X/1000., r.GetDirection.Y/1000., r.GetDirection.Z/1000.)
                                           let newOrigin = (r.PointAtTime s2T).Move moveVector
                                           this.subtractionHitFunction (new Ray(newOrigin, r.GetDirection))
        |(s1T, s2T) when s1T = s2T -> s1Hit
        |(s1T, s2T) -> if s1T < s2T then s1Hit
                       else
                           if (s1.isInside (r.PointAtTime s2T)) && (not (s2.isInside (r.PointAtTime s2T))) then s2Hit
                           else 
                               let moveVector = Vector(r.GetDirection.X/1000., r.GetDirection.Y/1000., r.GetDirection.Z/1000.)
                               let newOrigin = (r.PointAtTime s2T).Move moveVector
                               this.subtractionHitFunction (new Ray(newOrigin, r.GetDirection))
                               *)




    ////GROUPING////
    member this.groupingHitFunction (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r

        match (not(s1Hit.DidHit) && not(s1Hit.DidHit)) with
        |false ->
            let s1Time = match s1Hit.DidHit with
                         |true -> s1Hit.Time 
                         |false -> infinity
            let s2Time = match s2Hit.DidHit with 
                         |true -> s2Hit.Time 
                         |false -> infinity

            match (s1Time <= (s2Time + this.epsilon)) with 
            |true -> HitPoint(r, s1Hit.Time, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
            |false -> HitPoint(r, s2Hit.Time, s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
        |true -> HitPoint(r)

    
    ////GENERAL HIT-FUNCTION////
    override this.hitFunction (r:Ray) = match op with
                                        |Union -> this.unionHitFunction r
                                        |Intersection -> this.intersectionHitFunction r r //because they need the original ray, to calculate correct t-value
                                        |Subtraction -> this.subtractionHitFunction r r //because they need the original ray, to calculate correct t-value
                                        |Grouping -> this.groupingHitFunction r
                                        
