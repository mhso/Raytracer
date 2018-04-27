namespace Tracer.Basics
open System
open Tracer.Sampling

exception LightException

[<AbstractClass>]
type Material() = 
    abstract member Bounce: Shape -> HitPoint -> Light -> Colour
    abstract member AmbientColour: Shape -> HitPoint -> Colour
    abstract member Bounces: int
    abstract member BounceMethod: HitPoint -> Ray[]
    member this.IsRecursive = this.Bounces > 0
    member this.PreBounce (shape: Shape) (hitPoint: HitPoint) (light: Light) = 
        if light :? AmbientLight then
            this.AmbientColour shape hitPoint * light.Intensity
        else
            this.Bounce shape hitPoint light
    static member None = 
        MatteMaterial(Colour.White)

//- MATTE MATERIAL
and MatteMaterial(colour:Colour) = 
    inherit Material()

    let colour = colour
    let coefficient = 1.
    
    default this.Bounces = 0
    default this.BounceMethod hitPoint = [| hitPoint.Ray |]
    member this.Colour = colour
    member this.Coefficient = coefficient
    default this.AmbientColour shape hitPoint = colour
    
    default this.Bounce (shape: Shape) (hitPoint: HitPoint) (light: Light) = 
        
        // Initialize parameters 
        let kd  = coefficient                           // Matte coefficient
        let cd  = colour                                // Matte colour
        let lc:Colour  = light.GetColour hitPoint.Point // Light colour
        let n   = hitPoint.Normal                       // Normal at hit point
        let ld  = (light.GetDirectionFromPoint hitPoint.Point)  // Light direction

        // Determine the colour
        if n * ld > 0. then
            let friction    = (kd * cd) / Math.PI           
            let direction   = lc * (n * ld)                 
            friction * direction
        else
            Colour.Black

//- SPECULAR REFLECTION MATERIAL (PHONG)     
and SpecularMaterial 
    (
        specularCoefficient: float, 
        specularColour: Colour, 
        specularExponent: float, 
        matteColour: Colour
    ) = 
    inherit MatteMaterial(matteColour)
    let specularColour = specularColour
    let matteColour = matteColour
    let matteMaterial = new MatteMaterial(matteColour)
    default this.Bounces = 0
    default this.BounceMethod hitPoint = [| hitPoint.Ray |]
    default this.AmbientColour shape hitPoint = matteColour
    member this.SpecularCoefficient = specularCoefficient
    member this.SpecularColour = specularColour
    member this.MatteColour = matteColour
    member this.MatteMaterial = matteMaterial
    default this.Bounce (shape: Shape) (hitPoint: HitPoint) (light: Light) = 
        
        // Initialize parameters
        let kd = matteMaterial.Coefficient             // Matte coefficient
        let cd = matteMaterial.Colour                   // Matte colour
        let ld  = 
            let l:Vector = (light.GetDirectionFromPoint hitPoint.Point)
            l.Normalise // Light direction
        let n = hitPoint.Normal                         // Normal at hit point
        let r1 = -ld + (2. * (n * ld)) * n             // Light ray direction
        let ray:Ray = hitPoint.Ray
        let rd = ray.GetDirection             // Direction of ray
        let e = specularExponent                       // Specular exponent
        let ks = specularCoefficient                   // Specular coefficient
        let cs = specularColour                        // Specular colour
        let lc  = light.GetColour hitPoint.Point       // Light colour
        
        // Detemine the colour
        if n * ld > 0. then

            // The standard diffuse colour
            let matte = matteMaterial.Bounce shape hitPoint light
            
            // The specular colour
            let specular = 
                if r1 * -rd > 0. then
                    ks * cs * ((r1 * (-rd)) ** e)
                else
                    Colour.Black
            let direction = lc * (n * ld)
            
            // The final colour
            (matte + specular) * direction
        else
            Colour.Black

and PerfectReflectionMaterial(bounces: int, baseMaterial: Material, reflectionColour: Colour, reflectionCoefficient: float) =
    inherit Material()

    member this.BaseMaterial = baseMaterial                     // Material to apply perfect reflection to
    member this.ReflectionCoefficient = reflectionCoefficient   // Reflection coefficient
    member this.ReflectionColour = reflectionColour             // Reflection colour
    
    default this.AmbientColour shape hitPoint = baseMaterial.AmbientColour shape hitPoint     
    default this.Bounces = bounces                               
    default this.BounceMethod h = 
        let rayDirection = (h.Ray.GetDirection + (-2. * (h.Normal * h.Ray.GetDirection)) * h.Normal)
        [| Ray(h.Point, rayDirection) |]
    default this.Bounce (shape: Shape) (hitPoint: HitPoint) (light: Light) = 
        baseMaterial.Bounce shape hitPoint light

and GlossyMaterial(reflectionCoefficient: float, reflectionColour: Colour, baseMaterial: Material, sampleCount: int, setCount: int, bounces: int, sharpness: float) = 
    inherit Material()
    
    let random = new Random()
    let hemispheres = 
        Sampling.multiJittered sampleCount setCount
        |> Array.map (fun a -> Sampling.mapToHemisphere a sharpness)

    // Will reflect a ray along a hemisphere
    default this.Bounces = bounces
    default this.BounceMethod hitPoint =
        let direction = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal
        let rays = Array.create sampleCount Ray.None

        for i = 0 to sampleCount-1 do
            let hemisphere = hemispheres.[random.Next(0, hemispheres.Length - 1)]
            let sp = new Point(hemisphere.[random.Next(0, hemisphere.Length - 1)])
            let m = direction + 2. * (normal * -direction) * normal
            let up = new Vector(0., 1., 0.)
            let w = m.Normalise
            let v = (up % w).Normalise
            let u = w % v
        
            let apply_of = sp.OrthonormalTransform (u, v, w)
            rays.[i] <- if apply_of * normal > 0. then
                            Ray(hitPoint.Point, apply_of)
                        else
                            Ray(hitPoint.Point, -sp.X * u - sp.Y * v + sp.Z * w)
        rays

    member this.ReflectionCoefficient = reflectionCoefficient
    member this.BaseMaterial = baseMaterial
    default this.AmbientColour shape hitPoint = baseMaterial.AmbientColour shape hitPoint
    default this.Bounce (shape: Shape) (hitPoint: HitPoint) (light: Light) = 
        baseMaterial.Bounce shape hitPoint light
        
//- HITPOINT
and HitPoint(ray: Ray, time: float, normal: Vector, material: Material, didHit: bool) = 
    
    member this.Ray = ray
    member this.Time = time
    member this.Point: Point = ray.PointAtTime time
    member this.DidHit = didHit
    member this.Normal = normal
    member this.Material = material

    // For hit rays
    new(ray: Ray, time:float, normal: Vector, material: Material) = 
        HitPoint(ray, time, normal, material, true)

    // For missed rays
    new(ray: Ray) = HitPoint(ray, 0., new Vector(0.,0.,0.), Material.None , false)


//- LIGHT
and [<AbstractClass>] Light(colour: Colour, intensity: float) =
    let colour = colour
    let intensity = intensity
    member this.BaseColour = colour
    member this.Intensity: float = intensity

    // (l_c) Final colour
    abstract member GetColour: Point -> Colour

    // (l_d) Direction from a point to this light
    abstract member GetDirectionFromPoint: Point -> Vector

    // (_ls) Shadow ray
    abstract member GetShadowRay: HitPoint -> Ray

    // (l_G) Geometric factor
    abstract member GetGeometricFactor: Point -> float

    // (l_pdf) Probability density function
    abstract member GetProbabilityDensity: float



//- AMBIENT LIGHT
and AmbientLight(colour: Colour, intensity: float) =
    inherit Light(colour, intensity)

    override this.GetColour point = 
        new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint (point:Point) = 
        raise LightException
    override this.GetShadowRay (hitPoint:HitPoint) = 
        raise LightException
    override this.GetGeometricFactor point = 
        1.
    override this.GetProbabilityDensity = 
        1.


//- POINT LIGHT
and PointLight(colour: Colour, intensity: float, position: Point) = 
    inherit Light(colour, intensity)

    member this.Position = position

    override this.GetColour point = 
        new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint (point:Point) = 
        (position - point).Normalise
    override this.GetShadowRay (hitPoint:HitPoint) = 
        let normal:Vector = hitPoint.Normal
        let shadowRayOrigin = hitPoint.Point + normal * 0.00001
        let direction = (position - shadowRayOrigin).Normalise
        new Ray((shadowRayOrigin), direction)
    override this.GetGeometricFactor point = 
        1.
    override this.GetProbabilityDensity = 
        1.


//- DIRECTIONAL LIGHT  
and DirectionalLight(colour: Colour, intensity: float, direction: Vector) = 
    inherit Light(colour, intensity)

    member this.Direction = direction

    override this.GetColour point = 
        new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint (point:Point) = 
        direction.Normalise
    override this.GetShadowRay (hitPoint:HitPoint) = 
        new Ray(hitPoint.Point, direction.Normalise)
    override this.GetGeometricFactor point = 
        1.
    override this.GetProbabilityDensity = 
        1.

//- SHAPES (everything below is made by Alexander)
and Shape() =
    member this.hitFunction (ray:Ray) = 
        let hit = 
            match this with 
            | :? Rectangle as s -> s.hitFunction ray
            | :? Disc as      s -> s.hitFunction ray
            | :? Triangle as  s -> s.hitFunction ray
            | :? SphereShape as s -> s.hitFunction ray
            | :? HollowCylinder as s -> s.hitFunction ray
            | :? SolidCylinder as s -> s.hitFunction ray
            | :? Box as s -> s.hitFunction ray
            | :? InfinitePlane as s -> s.hitFunction ray
            | :? TransformShape as s -> s.hitFunction ray

        match hit with
            | (Some(time),Some(normal),Some(material)) -> 
                HitPoint(ray, time, normal, material)
            | (None,None,None) -> 
                HitPoint(ray)
    
and Rectangle(bottomLeft:Point, topLeft:Point, bottomRight:Point, tex:Material)=
    inherit Shape()
    member this.bottomleft = bottomLeft
    member this.topLeft = topLeft
    member this.bottomRight = bottomRight
    member this.tex = tex
    member this.width = bottomRight.X - bottomLeft.X
    member this.height = topLeft.Y - bottomLeft.Y
    member this.normal:Vector = new Vector(0.0, 0.0, 1.0)
    member this.hitFunction (r:Ray) = 
        match r with
            |(r) when (r.GetDirection.Z) = 0.0 -> (None, None, None) //This method checks if dz = 0.0, which would make the ray, parrallel to the plane 
            |(r) when (-((r.GetOrigin.Z) / (r.GetDirection.Z))) <= 0.0 -> (None, None, None) //This checks if t is 0 or smaller, in which case there is no hit
            |(r) -> let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z)))
                    let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                    let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                    if (px > 0.0 && px < this.width) && (py > 0.0 && py < this.height) 
                    then (Some(t),Some(new Vector(0.0, 0.0, 1.0)),Some(tex)) else (None, None, None)


                                                        
and Disc(center:Point, radius:float, tex:Material)=
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.tex = tex
    member this.normal: Vector = new Vector(0.0, 0.0, 1.0)
    member this.hitFunction (r:Ray) = 
        match r with
            |(r) when (r.GetDirection.Z) = 0.0 -> (None, None, None) //This method checks if dz = 0.0, which would make the ray, parrallel to the plane 
            |(r) when (-((r.GetOrigin.Z) / (r.GetDirection.Z))) <= 0.0 -> (None, None, None) //This checks if t is 0 or smaller, in which case there is no hit
            |(r) -> let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z)))
                    let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                    let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                    if (((px*px)+(py*py)) <= radius*radius) 
                        then (Some(t),Some(new Vector(0.0, 0.0, 1.0)),Some(tex)) else (None, None, None) //needs to return texture somehow
    



and Triangle(a:Point, b:Point, c:Point, mat:Material)=
    inherit Shape()
    member this.a = a
    member this.b = b
    member this.c = c
    member this.mat = mat
    member this.u = a-b //in case of errors try swithing a and b around
    member this.v = a-c // same here

    //the many let statements are fo simplifying cramers rule
    member this.hitFunction (r:Ray) = 
        let pa = ((a.X)-(b.X))
        let pb = ((a.X)-(c.X))
        let e = ((a.Y)-(b.Y))
        let f = ((a.Y)-(c.Y))
        let i = ((a.Z)-(b.Z))
        let j = ((a.Z)-(c.Z))

        match r with
            |(r) when (pa*((f*(r.GetDirection.Z))-((r.GetDirection.Y)*j)) + pb*(((r.GetDirection.Y)*i)-(e*(r.GetDirection.Z))) + (r.GetDirection.X)*((e*j)-(f*i))) = 0.0 -> (None, None, None)
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
                            then (Some(z), Some((this.u % this.v).Normalise), Some(mat)) else (None, None, None) //why mat instead of texture


and SphereShape(origin: Point, radius: float, tex: Material) = 
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

    member this.hitFunction (r:Ray) = 
        let D = this.GetDiscriminant r
        if D < 0. then (None, None, None)
        else
            let s = (r.GetOrigin - origin)
            let rayDir = r.GetDirection.Normalise
            let sv = s * rayDir
            let ss = s * s
            let (t1,t2) = (-sv + Math.Sqrt(D), -sv - Math.Sqrt(D))
            if t1 < 0. && t2 < 0. then(None, None, None)
            elif t1 < t2 then (Some(t1), Some(this.NormalAtPoint (r.PointAtTime t1)), Some(tex)) 
            else (Some(t2), Some(this.NormalAtPoint (r.PointAtTime t2)), Some(tex))



and HollowCylinder(center:Point, radius:float, height:float, tex:Material) = //change back to texture
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    member this.tex = tex

    member this.determineHitPoint (r:Ray) (t:float) = 
        let p = r.PointAtTime t
        if p.Y > -(height/2.0) && p.Y < (height/2.0) then (Some(t), Some(new Vector(p.X/radius, 0.0, p.Z/radius)), Some(tex)) else (None, None, None)

    member this.hitFunction (r:Ray) = 
        let a = ((r.GetDirection.X)**2.0) + ((r.GetDirection.Z)**2.0) //both are to the power of 2
        let b = 2.0*((r.GetOrigin.X * r.GetDirection.X)+(r.GetOrigin.Z * r.GetDirection.Z))
        let c = ((r.GetOrigin.X)**2.0) + ((r.GetOrigin.Z)**2.0) - (radius**2.0)
        let D = (b**2.0) - 4.0*a*c
        let t1 = (-b + Math.Sqrt(D))/(2.0 * a)
        let t2 = (-b - Math.Sqrt(D))/(2.0 * a)
        match D with
        |(0.0) -> match (t1,t2) with
                  |(t1,t2) when t1 <= 0.0 && t2 <= 0.0 -> (None, None, None)
                  |(t1,t2) -> if t1 < t2 && t1 > 0.0 then this.determineHitPoint r t1 else this.determineHitPoint r t2
        |(D) when D < 0.0 -> (None, None, None)
        |(D) -> match (t1,t2) with //when D > 0.0, and there are two valid values for t
                  |(t1,t2) when t1 <= 0.0 && t2 <= 0.0 -> (None, None, None)
                  |(t1,t2) -> if t1 < t2 && t1 > 0.0 then this.determineHitPoint r t1 else  if t2 > 0.0 then this.determineHitPoint r t2 
                                                                                            else this.determineHitPoint r t1

and SolidCylinder(center:Point, radius:float, height:float, cylinder:Material, top:Material, bottom:Material) =
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    member this.cylinder = cylinder
    member this.top = top
    member this.bottom = bottom

    member this.hitFunction (r:Ray) = (None, None, None)
    //affine transformation is needed for moving the disks


and Box(low:Point, high:Point, front:Material, back:Material, top:Material, bottom:Material, left:Material, right:Material) = 
    inherit Shape()
    member this.low = low
    member this.high = high
    member this.front = front
    member this.back = back
    member this.top = top
    member this.bottom = bottom
    member this.left = left
    member this.right = right

    member this.hitFunction (r:Ray) = 
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
                |(tx,ty,tz) when tx >= ty && tx >= tz -> if r.GetDirection.X > 0.0 then (Some(t), Some(Vector(-1.0, 0.0, 0.0)), Some(left)) //when tx is the biggest and t > 0.0
                                                         else (Some(t), Some(Vector(1.0, 0.0, 0.0)), Some(right))
                |(tx,ty,tz) when ty >= tx && ty >= tz -> if r.GetDirection.Y > 0.0 then (Some(t), Some(Vector(0.0, -1.0, 0.0)), Some(bottom)) //when ty is the biggest and t > 0.0
                                                         else (Some(t), Some(Vector(0.0, 1.0, 0.0)), Some(top))
                |(tx,ty,tz) when tz >= tx && tz >= ty -> if r.GetDirection.Z > 0.0 then (Some(t), Some(Vector(0.0, 0.0, -1.0)), Some(back)) //when tz is the biggest and t > 0.0
                                                         else (Some(t), Some(Vector(0.0, 0.0, 1.0)), Some(front))
            else
                match (tx', ty', tz') with
                |(tx',ty',tz') when tx' <= ty' && tx' <= tz' -> if r.GetDirection.X > 0.0 then (Some(t), Some(Vector(1.0, 0.0, 0.0)), Some(right)) //when tx' is the smallest and t > 0.0
                                                                else (Some(t'), Some(Vector(-1.0, 0.0, 0.0)), Some(left))
                |(tx',ty',tz') when ty' <= tx' && ty' <= tz' -> if r.GetDirection.Y > 0.0 then (Some(t), Some(Vector(0.0, 1.0, 0.0)), Some(top)) //when ty' is the smallest and t > 0.0
                                                                else (Some(t'), Some(Vector(0.0, -1.0, 0.0)), Some(bottom))
                |(tx',ty',tz') when tz' <= tx' && tz' <= ty' -> if r.GetDirection.Z > 0.0 then (Some(t), Some(Vector(0.0, 0.0, 1.0)), Some(front)) //when tz' is the smallest and t > 0.0
                                                                else (Some(t'), Some(Vector(0.0, 0.0, -1.0)), Some(back))
        else (None, None, None)
        

and InfinitePlane(tex:Material) = 
    inherit Shape()
    member this.tex = tex
    member this.hitFunction (r:Ray) = 
        let t = -(r.GetOrigin.Z / r.GetDirection.Z)
        if r.GetDirection.Z <> 0.0 && t > 0.0 then (Some(t), Some(new Vector(0.0, 0.0, 1.0)), Some(tex)) else (None, None, None)

and TransformShape (hitFunction) =
    inherit Shape()
    member this.hitFunction = hitFunction