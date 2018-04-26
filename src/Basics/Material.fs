namespace Tracer.Basics
open System
open Tracer.Sampling

[<AbstractClass>]
type Material() = 
    abstract member Bounce: Shape -> Shape list -> HitPoint -> Light -> Colour
    abstract member AmbientColour: Shape -> HitPoint -> Colour
    member this.PreBounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 
        
        if light :? AmbientLight then
            // If the light is ambient, simply add the colour
            light.GetColour * this.AmbientColour shape hitPoint
        else
            // Check if the shape is in the shadow
            let shadowRay: Ray = light.GetShadowRay hitPoint
            let (_, shadowHitPoint: HitPoint) = shadowRay.GetFirstHitPointExcept shapes shape
            if shadowHitPoint.DidHit then
                // It is a shadow
                (this.Bounce shape shapes hitPoint light).Scale(0.2)
                
            else
                // It is not a shadow
                this.Bounce shape shapes hitPoint light
    static member None = 
        MatteMaterial(Colour.White)

//- MATTE MATERIAL
and MatteMaterial(colour:Colour) = 
    inherit Material()

    let colour = colour
    let coefficient = 1.
    
    member this.Colour = colour
    member this.Coefficient = coefficient
    default this.AmbientColour shape hitPoint = colour
    
    default this.Bounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 
        
        // Initialize parameters 
        let kd  = coefficient                           // Matte coefficient
        let cd  = colour                                // Matte colour
        let lc:Colour  = light.GetColour                // Light colour
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
    default this.AmbientColour shape hitPoint = matteColour
    member this.SpecularCoefficient = specularCoefficient
    member this.SpecularColour = specularColour
    member this.MatteColour = matteColour
    member this.MatteMaterial = matteMaterial
    default this.Bounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 
        
        // Initialize parameters
        let kd = matteMaterial.Coefficient             // Matte coefficient
        let cd = matteMaterial.Colour                  // Matte colour
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
        let lc  = light.GetColour                      // Light colour
        
        // Detemine the colour
        if n * ld > 0. then

            // The standard diffuse colour
            let matte = matteMaterial.Bounce shape shapes hitPoint light
            
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



//- SPECULAR REFLECTION MATERIAL (BLINN-PHONG)    
//  Blinn-phong is another implementation of phong-shading
//  .. it looks a bit more smooth in my opinion.
and BlinnPhongMaterial (specularCoefficient: float, specularColour: Colour, specularExponent: float, matteColour: Colour) = 
    
    inherit SpecularMaterial(specularCoefficient, specularColour, specularExponent, matteColour)
    default this.Bounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 
        
        // The standard diffuse colour
        let diffuse = this.MatteMaterial.Bounce shape shapes hitPoint light
        
        // The specular colour
        let L:Vector = light.GetDirectionFromPoint hitPoint.Point
        let V:Vector = hitPoint.Point - hitPoint.Ray.GetOrigin
        let H = (L + V).Normalise
        let Is = Math.Pow(Math.Max(0.0, (H * hitPoint.Normal)), specularExponent)
        let specular = new Colour(1.0,1.0,1.0) * Is * specularCoefficient
        
        // The final colour
        diffuse + specular


//- PERFECT REFLECTION MATERIALS
and PerfectReflectionMaterial(bounces: int, baseMaterial: Material, reflectionColour: Colour, reflectionCoefficient: float) =
    inherit Material()
    default this.AmbientColour shape hitPoint = baseMaterial.AmbientColour shape hitPoint         
    member this.Bounces = bounces                               // Number of recursive bounces
    member this.BaseMaterial = baseMaterial                     // Material to apply perfect reflection to
    member this.ReflectionCoefficient = reflectionCoefficient   // Reflection coefficient
    member this.ReflectionColour = reflectionColour             // Reflection colour
    
    default this.Bounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 

        // Colour of the base material
        let baseColour = baseMaterial.Bounce shape shapes hitPoint light

        // Reflection function
        let reflectionFunction (direction: Vector) (normal: Vector) = 
            direction + (-2. * (normal * direction)) * normal

        // Colour of the perfect reflection
        let reflectedColour = hitPoint.Ray.CastRecursively shape shapes this hitPoint.Point hitPoint.Normal bounces light Colour.Black reflectionFunction

        // Final colour
        baseColour + this.ReflectionCoefficient * this.ReflectionColour * reflectedColour

//- GLOSSY MATERIALS
and GlossyMaterial(reflectionCoefficient: float, reflectionColour: Colour, baseMaterial: Material, sampleCount: int, setCount: int, bounces: int, sharpness: float) = 
    inherit Material()
    
    let random = new Random()
    let hemispheres = 
        Sampling.multiJittered sampleCount setCount
        |> Array.map (fun a -> Sampling.mapToHemisphere a sharpness)

    // Will reflect a ray along a hemisphere
    let hemisphereReflect (direction: Vector) (normal: Vector) =
        let hemisphere = hemispheres.[random.Next(0, hemispheres.Length - 1)]
        let sp = new Point(hemisphere.[random.Next(0, hemisphere.Length - 1)])
        let m = direction + 2. * (normal * -direction) * normal
        let up = new Vector(0., 1., 0.)
        let w = m.Normalise
        let v = (up % w).Normalise
        let u = w % v
        
        let apply_of = sp.OrthonormalTransform (u, v, w)
        if apply_of * normal > 0. then
            apply_of
        else
            -sp.X * u - sp.Y * v + sp.Z * w        
    
    member this.ReflectionCoefficient = reflectionCoefficient
    member this.BaseMaterial = baseMaterial
    default this.AmbientColour shape hitPoint = Colour.White
    default this.Bounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 

        // Colour of the base material
        let baseColour = baseMaterial.Bounce shape shapes hitPoint light

        // Reflection function
        let reflectionFunction (direction: Vector) (normal: Vector) = 
            hemisphereReflect direction normal

        // Colour of the perfect reflection
        let totalColours = 
            [for i in 1..sampleCount do yield hitPoint.Ray.CastRecursively shape shapes this hitPoint.Point hitPoint.Normal bounces light Colour.Black reflectionFunction]
            |> List.fold (fun acc a -> acc + a) Colour.Black
        let reflectedColour:Colour = totalColours / float(sampleCount)

        // Final colour
        baseColour + this.ReflectionCoefficient * reflectionColour * reflectedColour

and TexturedMaterial(baseMaterial: Material, textureFilePath: string) = 
    inherit Material()

    let bitmap = new Drawing.Bitmap(textureFilePath)
    
    let getUVPixel u v = 
        let relativeU = u % 1.
        let relativeV = v % 1.
        let (x,y) = relativeU * float(bitmap.Width - 1), (1. - relativeV) * float(bitmap.Height - 1)
        Colour(bitmap.GetPixel(Convert.ToInt32(x),Convert.ToInt32(y)))
        
    member this.BaseMaterial = baseMaterial
    member this.TextureFilePath = textureFilePath
    member this.Bitmap = bitmap

    default this.AmbientColour shape hitPoint = 
        let d:Vector = hitPoint.Normal
        let u = 0.5 + (atan2 d.Z d.X) / (2. * Math.PI)
        let v = 0.5 - (asin d.Y) / Math.PI
        getUVPixel u v

    default this.Bounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 
        let baseColour = baseMaterial.Bounce shape shapes hitPoint light
        let d:Vector = hitPoint.Normal
        let u = 0.5 + (atan2 d.Z d.X) / (2. * Math.PI)
        let v = 0.5 - (asin d.Y) / Math.PI
        let uvColour = getUVPixel u v
        uvColour * baseColour

//- MIX TWO MATERIALS
and MixedMaterial(a: Material, b: Material, factor: float) =
    inherit Material()
    default this.AmbientColour shape hitPoint = (a.AmbientColour shape hitPoint).Scale(1.-factor) + (b.AmbientColour shape hitPoint).Scale(factor)
    member this.MaterialA = a
    member this.MaterialB = b
    member this.Factor = factor
    default this.Bounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 

        // Get the colour from first material
        let colorA = a.Bounce shape shapes hitPoint light

        // Get the colour from the second material
        let colorB = b.Bounce shape shapes hitPoint light

        // Combine the two in the balance of the factor
        colorA.Scale(1.-factor) + colorB.Scale(factor)

//- ADD TWO MATERIALS
and AddMaterial(a: Material, b: Material) = 
    inherit Material()
    default this.AmbientColour shape hitPoint = a.AmbientColour shape hitPoint + b.AmbientColour shape hitPoint
    member this.MaterialA = a
    member this.MaterialB = b
    default this.Bounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 

        // First material colour
        let colorA = a.Bounce shape shapes hitPoint light

        // Second material colour
        let colorB = b.Bounce shape shapes hitPoint light

        // Final colour
        colorA + colorB

//- USE CURRYING ON TWO MATERIALS
and CurryMaterial(curry: Colour -> Colour -> Colour, a: Material, b: Material) = 
    inherit Material()
    default this.AmbientColour shape hitPoint = curry (a.AmbientColour shape hitPoint) (b.AmbientColour shape hitPoint)
    member this.MaterialA = a
    member this.MaterialB = b
    default this.Bounce (shape: Shape) (shapes: Shape list) (hitPoint: HitPoint) (light: Light) = 

        // First material colour
        let colorA = a.Bounce shape shapes hitPoint light

        // Second material colour
        let colorB = b.Bounce shape shapes hitPoint light

        // Final colour
        curry colorA colorB

//- RAY
and Ray(origin: Point, direction: Vector) = 
    member this.GetOrigin = origin
    member this.GetDirection = direction.Normalise

    // Returns a point from a given time/length of the ray
    member this.PointAtTime (t:float) = 
        origin + t * direction

    // Returns a time/length from a given point of the ray
    member this.TimeAtPoint (p:Point) = 
        (p - origin).Z / direction.Z

    // Get the first point the ray hits (if it hits, otherwise an empty hit point)
    member this.GetFirstHitPoint (shapes: Shape list) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction this)]
                |> List.filter (fun (_,hp:HitPoint) -> hp.DidHit)
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape(), new HitPoint(this))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

     member this.GetFirstHitPointExcept (shapes: Shape list) (except: Shape) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction this)]
                |> List.filter (fun (_,hp) -> hp.DidHit)
                |> List.filter (fun (shape,_) -> 
                    let eq = Object.ReferenceEquals(shape, except)
                    not eq)

        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape(), new HitPoint(this))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

     // Returns the colour in the first hit point of the ray
     member this.Cast (backgroundColour: Colour) (lights: Light list) (shapes:Shape list) = 

        // Get the hitpoint
        let (shape, hitPoint) = this.GetFirstHitPoint shapes

        // Check if we hit
        if hitPoint.DidHit then

            // Sum the light colors for that hitpoint
            let normal = hitPoint.Normal
            lights 
                |> List.fold (fun accColour light -> 
                    let colour = 
                        let m: Material = hitPoint.Material
                        m.PreBounce shape shapes hitPoint light
                    accColour + colour) (new Colour(0.,0.,0.))
         else
            // If we did not hit, return the background colour
            backgroundColour

    // Will cast a ray recursively
    member this.CastRecursively 
        (shape: Shape) (shapes: Shape list) (material: Material) 
        (hitPoint: Point) (hitNormal: Vector) (bounces: int) (light: Light) (colour: Colour) 
        (reflectionFunction: Vector -> Vector -> Vector)=
        
        let baseBounce material = 
            // Get material of final shape
            let material = 
                match box material with
                | :? PerfectReflectionMaterial as p -> p.BaseMaterial
                | :? GlossyMaterial as p -> p.BaseMaterial
                | _ -> material

            let time = (hitPoint - origin).Magnitude
            let hit = HitPoint(this, time, hitNormal, material)

            // Get the colour of it
            material.PreBounce shape shapes hit light
        
        if bounces = 0 then
            baseBounce material
        else
            
            match box material with
                | :? PerfectReflectionMaterial
                | :? GlossyMaterial -> 
                    // Make a new, reflected ray
                    let newDirection = reflectionFunction direction hitNormal
                    let newOrigin = hitPoint
                    let newRay = new Ray(newOrigin + newDirection.Normalise * 0.000001, newDirection.Normalise)

                    // Check if it hit anything
                    let (newShape:Shape, newHitPoint:HitPoint) = newRay.GetFirstHitPointExcept shapes shape
                    if newHitPoint.DidHit then

                        // Cast rays recursively from the new hit point
                        let newNormal = newHitPoint.Normal
                        let newColour = baseBounce material
                        newRay.CastRecursively 
                            newShape shapes newHitPoint.Material 
                            newHitPoint.Point newNormal (bounces - 1) light newColour 
                            reflectionFunction

                    else
                        // The ray did not hit anything
                        colour
                | _ as p -> 
                    let time = (hitPoint - origin).Magnitude
                    let hit = HitPoint(this, time, hitNormal, material)
                    let baseColour = material.PreBounce shape shapes hit light
                    baseColour + colour

    member this.Invert = 
        new Ray(origin, direction.Invert)

//- HITPOINT
//  .. a hit point is the point where a ray meets a shape
and HitPoint(ray: Ray, time: float, normal: Vector, material: Material, didHit: bool) = 
    member this.Ray = ray
    member this.Time = time
    member this.Point: Point = ray.PointAtTime time
    member this.DidHit = didHit
    member this.Normal = normal
    member this.Material = material

    // Constructor for a hit point that hit something
    new(ray: Ray, time:float, normal: Vector, material: Material) = 
        HitPoint(ray, time, normal, material, true)

    // Constructor for a hit point that missed
    new(ray: Ray) = HitPoint(ray, 0., new Vector(0.,0.,0.), Material.None , false)

//- LIGHT
// .. Never instantiate this instance directly, always a subclass. 
// .. The class is meant to be abstract, but it was needed mutually recursively
and Light(colour: Colour, intensity: float) =
    let colour = colour
    let intensity = intensity
    member this.BaseColour = colour
    member this.Intensity = intensity

    // Returns the final colour of the light
    member this.GetColour = 
        let r = colour.R * intensity
        let g = colour.G * intensity
        let b = colour.B * intensity
        new Colour(r, g, b)

    // Returns the direction from a point to the light
    member this.GetDirectionFromPoint (point:Point) = 
        match this with
            | :? PointLight as p ->
                p.GetDirectionFromPoint point
            | :? DirectionalLight as p ->
                p.GetDirectionFromPoint point
            | _ -> raise LightException

    // Returns the shadow ray to the light
    // .. same as GetDirectionFromPoint, but inverted
    member this.GetShadowRay (hitPoint: HitPoint) = 
        match this with
            | :? PointLight as p ->
                p.GetShadowRay hitPoint
            | :? DirectionalLight as p ->
                p.GetShadowRay hitPoint
            | _ -> raise LightException



//- AMBIENT LIGHT
//.. read comments above for Light if in doubt
and AmbientLight(colour: Colour, intensity: float) =
    inherit Light(colour, intensity)
    member this.GetDirectionFromPoint (point:Point) = 
        raise LightException
    member this.GetShadowRay (hitPoint:HitPoint) = 
        raise LightException


//- POINT LIGHT
//.. read comments above for Light if in doubt
and PointLight(colour: Colour, intensity: float, position: Point) = 
    inherit Light(colour, intensity)
    let position = position
    member this.Position = position
    member this.GetDirectionFromPoint (point:Point) = 
        (position - point).Normalise
    member this.GetShadowRay (hitPoint:HitPoint) = 
        let normal:Vector = hitPoint.Normal
        let shadowRayOrigin = hitPoint.Point + normal * 0.00001
        let direction = (position - shadowRayOrigin).Normalise
        new Ray((shadowRayOrigin), direction)


//- DIRECTIONAL LIGHT  
//.. read comments above for Light if in doubt
and DirectionalLight(colour: Colour, intensity: float, direction: Vector) = 
    inherit Light(colour, intensity)
    let direction = direction
    member this.Direction = direction
    member this.GetDirectionFromPoint (point:Point) = 
        direction.Normalise
    member this.GetShadowRay (hitPoint:HitPoint) = 
        new Ray(hitPoint.Point, direction.Normalise)


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
    member this.bottomLeft = bottomLeft
    member this.topLeft = topLeft
    member this.bottomRight = bottomRight
    member this.tex = tex
    member this.width = bottomRight.X - bottomLeft.X
    member this.height = topLeft.Y - bottomLeft.Y
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
            if t1 < t2 then (Some(t1), Some(this.NormalAtPoint (r.PointAtTime t1)), Some(tex)) 
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