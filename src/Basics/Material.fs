namespace Tracer.Basics
open System
open Tracer.Sampling

[<AbstractClass>]
type Material() = 
    abstract member Bounce: Sphere -> Sphere list -> Point*Vector -> Ray -> Light -> Colour
    abstract member AmbientColour: Colour
    member this.PreBounce (sphere: Sphere) (spheres: Sphere list) (hitPoint: Point, normal: Vector) (ray: Ray) (light: Light) = 
        
        if light :? AmbientLight then
            // If the light is ambient, simply add the colour
            light.GetColour * this.AmbientColour
        else
            // Check if the shape is in the shadow
            let shadowRay: Ray = light.GetShadowRay hitPoint sphere
            let (_, shadowHitPoint: HitPoint) = shadowRay.GetFirstHitPointExcept spheres sphere
            if shadowHitPoint.DidHit then
                // It is a shadow
                (this.Bounce sphere spheres (hitPoint, normal) ray light).Scale(0.2)
                
            else
                // It is not a shadow
                this.Bounce sphere spheres (hitPoint, normal) ray light

//- MATTE MATERIAL
and MatteMaterial(colour:Colour) = 
    inherit Material()

    let colour = colour
    let coefficient = 1.
    
    member this.Colour = colour
    member this.Coefficient = coefficient
    default this.AmbientColour = colour
    
    default this.Bounce (sphere: Sphere) (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        
        // Initialize parameters 
        let kd  = coefficient                           // Matte coefficient
        let cd  = colour                                // Matte colour
        let lc:Colour  = light.GetColour                       // Light colour
        let n   = normalHitPoint                        // Normal at hit point
        let ld  = (light.GetDirectionFromPoint hitPoint sphere)  // Light direction

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
    let specularCoefficient = 1.
    let specularColour = specularColour
    let matteColour = matteColour
    let matteMaterial = new MatteMaterial(matteColour)
    default this.AmbientColour = matteColour
    member this.SpecularCoefficient = specularCoefficient
    member this.SpecularColour = specularColour
    member this.MatteColour = matteColour
    member this.MatteMaterial = matteMaterial
    default this.Bounce (sphere: Sphere) (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        
        // Initialize parameters
        let kd = matteMaterial.Coefficient             // Matte coefficient
        let cd = matteMaterial.Colour                  // Matte colour
        let ld  = (light.GetDirectionFromPoint hitPoint sphere).Normalise // Light direction
        let n = normalHitPoint                         // Normal at hit point
        let r1 = -ld + (2. * (n * ld)) * n             // Light ray direction
        let rd = ray.GetDirection                      // Direction of ray
        let e = specularExponent                       // Specular exponent
        let ks = specularCoefficient                   // Specular coefficient
        let cs = specularColour                        // Specular colour
        let lc  = light.GetColour                      // Light colour
        
        // Detemine the colour
        if n * ld > 0. then

            // The standard diffuse colour
            let matteFriction = matteMaterial.Bounce sphere spheres (hitPoint, normalHitPoint) ray light
            
            // The specular colour
            let specularFriction = 
                if r1 * -rd > 0. then
                    ks * cs * ((r1 * (-rd)) ** e)
                else
                    Colour.Black
            let direction = lc * (n * ld)
            
            // The final colour
            (matteFriction + specularFriction) * direction
        else
            Colour.Black



//- SPECULAR REFLECTION MATERIAL (BLINN-PHONG)    
//  Blinn-phong is another implementation of phong-shading
//  .. it looks a bit more smooth in my opinion.
and BlinnPhongMaterial (specularCoefficient: float, specularColour: Colour, specularExponent: float, matteColour: Colour) = 
    
    inherit SpecularMaterial(specularCoefficient, specularColour, specularExponent, matteColour)
    default this.Bounce (sphere: Sphere) (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        
        // The standard diffuse colour
        let diffuse = this.MatteMaterial.Bounce sphere spheres (hitPoint, normalHitPoint) ray light
        
        // The specular colour
        let L:Vector = light.GetDirectionFromPoint hitPoint sphere
        let V:Vector = hitPoint - ray.GetOrigin
        let H = (L + V).Normalise
        let Is = Math.Pow(Math.Max(0.0, (H * normalHitPoint)), specularExponent)
        let specular = new Colour(1.0,1.0,1.0) * Is * specularCoefficient
        
        // The final colour
        diffuse + specular


//- PERFECT REFLECTION MATERIALS
and PerfectReflectionMaterial(bounces: int, baseMaterial: Material, reflectionColour: Colour, reflectionCoefficient: float) =
    inherit Material()
    default this.AmbientColour = baseMaterial.AmbientColour                   
    member this.Bounces = bounces                               // Number of recursive bounces
    member this.BaseMaterial = baseMaterial                     // Material to apply perfect reflection to
    member this.ReflectionCoefficient = reflectionCoefficient   // Reflection coefficient
    member this.ReflectionColour = reflectionColour             // Reflection colour
    
    default this.Bounce (sphere: Sphere) (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 

        // Colour of the base material
        let baseColour = baseMaterial.Bounce sphere spheres (hitPoint,normalHitPoint) ray light

        // Reflection function
        let reflectionFunction (direction: Vector) (normal: Vector) = 
            direction + (-2. * (normal * direction)) * normal

        // Colour of the perfect reflection
        let reflectedColour = ray.CastRecursively sphere spheres this hitPoint normalHitPoint bounces light Colour.Black reflectionFunction

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
    default this.AmbientColour = Colour.White
    default this.Bounce (sphere: Sphere) (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 

        // Colour of the base material
        let baseColour = baseMaterial.Bounce sphere spheres (hitPoint,normalHitPoint) ray light

        // Reflection function
        let reflectionFunction (direction: Vector) (normal: Vector) = 
            hemisphereReflect direction normal

        // Colour of the perfect reflection
        let totalColours = 
            [for i in 1..sampleCount do yield ray.CastRecursively sphere spheres this hitPoint normalHitPoint bounces light Colour.Black reflectionFunction]
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

    default this.AmbientColour = baseMaterial.AmbientColour
    default this.Bounce (sphere: Sphere) (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        let d:Vector = sphere.NormalAtPoint hitPoint
        let u = 0.5 + (atan2 d.Z d.X) / (2. * Math.PI)
        let v = 0.5 - (asin d.Y) / Math.PI
        let baseColour = baseMaterial.Bounce sphere spheres (hitPoint,normalHitPoint) ray light
        let uvColour = getUVPixel u v
        baseColour + uvColour

//- MIX TWO MATERIALS
and MixedMaterial(a: Material, b: Material, factor: float) =
    inherit Material()
    default this.AmbientColour = a.AmbientColour.Scale(1.-factor) + b.AmbientColour.Scale(factor)
    member this.MaterialA = a
    member this.MaterialB = b
    member this.Factor = factor
    default this.Bounce (sphere: Sphere) (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 

        // Get the colour from first material
        let colorA = a.Bounce sphere spheres (hitPoint, normalHitPoint) ray light

        // Get the colour from the second material
        let colorB = b.Bounce sphere spheres (hitPoint, normalHitPoint) ray light

        // Combine the two in the balance of the factor
        colorA.Scale(1.-factor) + colorB.Scale(factor)

        
//- SPHERE (for testing purposes only)
and Sphere(origin: Point, radius: float, material: Material) = 
    member this.Origin = origin
    member this.Radius = radius
    member this.Material = material

    // Returns the normal vector from a hit point
    member this.NormalAtPoint (p:Point) = 
        (p - origin).Normalise

    // Returns the discriminant from a ray
    member this.GetDiscriminant (ray:Ray) = 
        let s = (ray.GetOrigin - origin)
        let rayDir = ray.GetDirection
        let sv = s * rayDir
        let ss = s * s
        (sv*sv) - ss + (radius*radius)

    // Returns the first hit point of a ray
    member this.GetHitPoint (ray:Ray) = 
        let D = this.GetDiscriminant ray
        if D < 0. then
            new HitPoint(ray)
        else
            let s = (ray.GetOrigin - origin)
            let rayDir = ray.GetDirection
            let sv = s * rayDir
            let (t1,t2) = (-sv + Math.Sqrt(D), -sv - Math.Sqrt(D))
            if t1 < 0.0 && t2 < 0.0 then
                HitPoint(ray)
            else
                let p = ray.PointAtTime (if t1 <= t2 then t1 else t2)
                HitPoint(ray, if t1 <= t2 then t1 else t2)
    
    // Same as GetHitPoint, but accepts negative ray lengths
    member this.GetHitPointBidirectional (ray:Ray) = 
        let D = this.GetDiscriminant ray
        if D < 0. then
            new HitPoint(ray)
        else
            let s = (ray.GetOrigin - origin)
            let rayDir = ray.GetDirection
            let sv = s * rayDir
            let (t1,t2) = (-sv + Math.Sqrt(D), -sv - Math.Sqrt(D))
            let p = ray.PointAtTime (if t1 <= t2 then t1 else t2)
            HitPoint(ray, if t1 <= t2 then t1 else t2)

    // A default sphere. Avoid using where possible. 
    static member None = 
        new Sphere(new Point(0.,0.,0.), 0., new MatteMaterial(Colour.Black))


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
    member this.GetFirstHitPoint (spheres: Sphere list) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in spheres do yield (s, s.GetHitPoint this)]
                |> List.filter (fun (_,hp) -> hp.DidHit)
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Sphere.None, new HitPoint(this))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

     member this.GetFirstHitPointBidirectional (spheres: Sphere list) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in spheres do yield (s, s.GetHitPointBidirectional this)]
                |> List.filter (fun (_,hp) -> hp.DidHit)
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Sphere.None, new HitPoint(this))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

     member this.GetFirstHitPointExcept (spheres: Sphere list) (except: Sphere) = 
        // Get all hit points
        let pointsThatHit = 
            [for s in spheres do yield (s, s.GetHitPoint this)]
                |> List.filter (fun (_,hp) -> hp.DidHit)
                |> List.filter (fun (sphere,_) -> 
                    let eq = Object.ReferenceEquals(sphere, except)
                    not eq)

        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Sphere.None, new HitPoint(this))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

     // Returns the colour in the first hit point of the ray
     member this.Cast (backgroundColour: Colour) (lights: Light list) (spheres:Sphere list) = 

        // Get the hitpoint
        let (sphere, hitPoint) = this.GetFirstHitPoint spheres

        // Check if we hit
        if hitPoint.DidHit then

            // Sum the light colors for that hitpoint
            let normal = sphere.NormalAtPoint hitPoint.Point
            lights 
                |> List.fold (fun accColour light -> 
                    let colour = sphere.Material.PreBounce sphere spheres (hitPoint.Point, normal) this light
                    accColour + colour) (new Colour(0.,0.,0.))
         else
            // If we did not hit, return the background colour
            backgroundColour

    // Will cast a ray recursively
    member this.CastRecursively 
        (sphere: Sphere) (spheres: Sphere list) (material: Material) 
        (hitPoint: Point) (hitNormal: Vector) (bounces: int) (light: Light) (colour: Colour) 
        (reflectionFunction: Vector -> Vector -> Vector)=
        
        let baseBounce material = 
            // Get material of final shape
            let material = 
                match box material with
                | :? PerfectReflectionMaterial as p -> p.BaseMaterial
                | :? GlossyMaterial as p -> p.BaseMaterial
                | _ -> material

            // Get the colour of it
            material.Bounce sphere spheres (hitPoint, hitNormal) this light
        
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
                    let (newSphere:Sphere, newHitPoint:HitPoint) = newRay.GetFirstHitPointExcept spheres sphere
                    if newHitPoint.DidHit then

                        // Cast rays recursively from the new hit point
                        let newNormal = newSphere.NormalAtPoint newHitPoint.Point
                        let newColour = baseBounce material
                        newRay.CastRecursively 
                            newSphere spheres newSphere.Material 
                            newHitPoint.Point newNormal (bounces - 1) light newColour 
                            reflectionFunction

                    else
                        // The ray did not hit anything
                        colour
                | _ as p -> 
                    let baseColour = material.Bounce sphere spheres (hitPoint, hitNormal) this light
                    baseColour + colour

    member this.Invert = 
        new Ray(origin, direction.Invert)

//- HITPOINT
//  .. a hit point is the point where a ray meets a shape
and HitPoint(ray: Ray, time: float, didHit: bool) = 
    member this.Ray = ray
    member this.Time = time
    member this.Point = ray.PointAtTime time
    member this.DidHit = didHit

    // Constructor for a hit point that hit something
    new(ray: Ray, time:float) = HitPoint(ray, time, true)

    // Constructor for a hit point that missed
    new(ray: Ray) = HitPoint(ray, 0., false)

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
    member this.GetDirectionFromPoint (hitPoint:Point) (sphere: Sphere) = 
        match this with
            | :? PointLight as p ->
                p.GetDirectionFromPoint hitPoint sphere
            | :? DirectionalLight as p ->
                p.GetDirectionFromPoint hitPoint sphere
            | _ -> raise LightException

    // Returns the shadow ray to the light
    // .. same as GetDirectionFromPoint, but inverted
    member this.GetShadowRay (hitPoint: Point) (sphere: Sphere) = 
        match this with
            | :? PointLight as p ->
                p.GetShadowRay hitPoint sphere
            | :? DirectionalLight as p ->
                p.GetShadowRay hitPoint sphere
            | _ -> raise LightException



//- AMBIENT LIGHT
//.. read comments above for Light if in doubt
and AmbientLight(colour: Colour, intensity: float) =
    inherit Light(colour, intensity)
    member this.GetDirectionFromPoint (point:Point) = 
        raise LightException
    member this.GetShadowRay (hitPoint:Point) = 
        raise LightException


//- POINT LIGHT
//.. read comments above for Light if in doubt
and PointLight(colour: Colour, intensity: float, position: Point) = 
    inherit Light(colour, intensity)
    let position = position
    member this.Position = position
    member this.GetDirectionFromPoint (hitPoint:Point) (sphere: Sphere) = 
        (position - hitPoint).Normalise
    member this.GetShadowRay (hitPoint:Point) (sphere: Sphere) = 
        let normal = sphere.NormalAtPoint hitPoint
        let shadowRayOrigin = hitPoint + normal * 0.00001
        new Ray((shadowRayOrigin),(this.GetDirectionFromPoint shadowRayOrigin sphere))


//- DIRECTIONAL LIGHT  
//.. read comments above for Light if in doubt
and DirectionalLight(colour: Colour, intensity: float, direction: Vector) = 
    inherit Light(colour, intensity)
    let direction = direction
    member this.Direction = direction
    member this.GetDirectionFromPoint (hitPoint:Point) (sphere: Sphere) = 
        direction.Normalise
    member this.GetShadowRay (hitPoint:Point) (sphere: Sphere) = 
        new Ray((hitPoint),(this.GetDirectionFromPoint hitPoint sphere))