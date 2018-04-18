namespace Tracer.Basics
open System

[<AbstractClass>]
type Material() = 
    abstract member Bounce: Sphere list -> Point*Vector -> Ray -> Light -> Colour
    abstract member AmbientColour: Colour
    member this.PreBounce (spheres: Sphere list) (hitPoint: Point, normal: Vector) (ray: Ray) (light: Light) = 
        if light :? AmbientLight then
            light.GetColour * this.AmbientColour
        else
            this.Bounce (spheres: Sphere list) (hitPoint, normal) ray light

//- MATTE MATERIAL
and MatteMaterial(colour:Colour) = 
    inherit Material()

    let colour = colour
    let coefficient = 1.
    
    member this.Colour = colour
    member this.Coefficient = coefficient
    default this.AmbientColour = colour
    
    default this.Bounce (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        
        // Initialize parameters 
        let kd  = coefficient                           // Matte coefficient
        let cd  = colour                                // Matte colour
        let lc:Colour  = light.GetColour                       // Light colour
        let n   = normalHitPoint                        // Normal at hit point
        let ld  = (light.GetDirectionFromPoint hitPoint)  // Light direction

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
    default this.Bounce (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        
        // Initialize parameters
        let kd = matteMaterial.Coefficient             // Matte coefficient
        let cd = matteMaterial.Colour                  // Matte colour
        let ld  = (light.GetDirectionFromPoint hitPoint).Normalise // Light direction
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
            let matteFriction = matteMaterial.Bounce spheres (hitPoint, normalHitPoint) ray light
            
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
    default this.Bounce (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        
        // The standard diffuse colour
        let diffuse = this.MatteMaterial.Bounce spheres (hitPoint, normalHitPoint) ray light
        
        // The specular colour
        let L = light.GetDirectionFromPoint hitPoint
        let V = new Vector(4., 0., 0.) - hitPoint
        let H = (L + V).Normalise
        let Is = Math.Pow(Math.Max(0.0, (H * normalHitPoint)), specularExponent)
        let specular = new Colour(1.0,1.0,1.0) * Is * specularCoefficient
        
        // The final colour
        diffuse + specular


//- PERFECT REFLECTION MATERIALS
and PerfectReflectionMaterial(bounces: int, baseMaterial: Material, reflectionColour: Colour, reflectionCoefficient: float) =
    inherit Material()
    default this.AmbientColour = Colour.White                   
    member this.Bounces = bounces                               // Number of recursive bounces
    member this.BaseMaterial = baseMaterial                     // Material to apply perfect reflection to
    member this.ReflectionCoefficient = reflectionCoefficient   // Reflection coefficient
    member this.ReflectionColour = reflectionColour             // Reflection colour

    // Will cast a ray recursively
    member this.CastRecursiveRay (spheres: Sphere list) (material: Material) (originalRay:Ray) (hitPoint: Point) (hitNormal: Vector) (bounces: int) (light: Light) =
        if bounces = 0 then

            // Get material of final shape
            let material = 
                match box material with
                | :? PerfectReflectionMaterial as p -> p.BaseMaterial
                | _ -> material

            // Get the colour of it
            material.PreBounce spheres (hitPoint, hitNormal) originalRay light

        else
            
            // Make a new, reflected ray
            let newDirection = -originalRay.GetDirection + (-2. * (hitNormal * -originalRay.GetDirection)) * hitNormal
            let newOrigin = hitPoint
            let newRay = new Ray(newOrigin, newDirection)

            // Check if it hit anything
            let (newSphere:Sphere, newHitPoint:HitPoint) = newRay.GetFirstHitPoint spheres
            if newHitPoint.DidHit then

                // Cast rays recursively from the new hit point
                let newNormal = newSphere.NormalAtPoint newHitPoint.Point
                this.CastRecursiveRay spheres newSphere.Material newRay newHitPoint.Point newNormal (bounces - 1) light

            else
                // The ray did not hit anything
                Colour.Black

    default this.Bounce (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 

        // Colour of the base material
        let baseColour = baseMaterial.Bounce spheres (hitPoint,normalHitPoint) ray light

        // Colour of the perfect reflection
        let reflectedColour = this.CastRecursiveRay spheres this ray hitPoint normalHitPoint bounces light

        // Final colour
        baseColour + this.ReflectionCoefficient * this.ReflectionColour * reflectedColour

//- MIX TWO MATERIALS
and MixedMaterial(a: Material, b: Material, factor: float) =
    inherit Material()
    default this.AmbientColour = Colour.White
    member this.MaterialA = a
    member this.MaterialB = b
    member this.Factor = factor
    default this.Bounce (spheres: Sphere list) (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 

        // Get the colour from first material
        let colorA = a.Bounce spheres (hitPoint, normalHitPoint) ray light

        // Get the colour from the second material
        let colorB = b.Bounce spheres (hitPoint, normalHitPoint) ray light

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
            let p = ray.PointAtTime (if t1 <= t2 then t1 else t2)
            let a = new HitPoint(ray, if t1 <= t2 then t1 else t2)
            a

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
                    let colour = sphere.Material.PreBounce spheres (hitPoint.Point, normal) this light
                    accColour + colour) (new Colour(0.,0.,0.))
         else
            // If we did not hit, return the background colour
            backgroundColour


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
    member this.GetDirectionFromPoint (hitPoint:Point) = 
        match this with
            | :? PointLight as p ->
                p.GetDirectionFromPoint hitPoint
            | :? DirectionalLight as p ->
                p.GetDirectionFromPoint hitPoint
            | _ -> raise LightException

    // Returns the shadow ray to the light
    // .. same as GetDirectionFromPoint, but inverted
    member this.GetShadowRay (hitPoint: Point) = 
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
    member this.GetShadowRay (hitPoint:Point) = 
        raise LightException


//- POINT LIGHT
//.. read comments above for Light if in doubt
and PointLight(colour: Colour, intensity: float, position: Point) = 
    inherit Light(colour, intensity)
    let position = position
    member this.Position = position
    member this.GetDirectionFromPoint (hitPoint:Point) = 
        (- (hitPoint - position)).Normalise
    member this.GetShadowRay (hitPoint:Point) = 
        new Ray((hitPoint),(this.GetDirectionFromPoint hitPoint).Invert)


//- DIRECTIONAL LIGHT  
//.. read comments above for Light if in doubt
and DirectionalLight(colour: Colour, intensity: float, direction: Vector) = 
    inherit Light(colour, intensity)
    let direction = direction
    member this.Direction = direction
    member this.GetDirectionFromPoint (hitPoint:Point) = 
        direction.Normalise
    member this.GetShadowRay (hitPoint:Point) = 
        new Ray((hitPoint),(this.GetDirectionFromPoint hitPoint).Invert)