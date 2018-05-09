namespace Tracer.Basics
open System
open Tracer.Sampling.Sampling
open System.Drawing


//- MATTE MATERIAL
type MatteMaterial
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float
    ) = 
    inherit Material()
    
    member this.MatteCoefficient = matteCoefficient
    member this.MatteColour = matteColour
    default this.AmbientColour = ambientColour * ambientCoefficient
    default this.ReflectionFactor = Colour.White
    default this.BounceMethod hitPoint = [||]
    default this.IsRecursive = false
    default this.Bounce(shape, hitPoint, light) = 
        
        // Initialize parameters 
        let ca = ambientColour                              // Ambient colour
        let ka = ambientCoefficient                         // Ambient coefficient
        let kd = matteCoefficient                           // Matte coefficient
        let cd = matteColour                                // Matte colour
        let lc = light.GetColour hitPoint                   // Light colour
        let n  = hitPoint.Normal                            // Normal at hit point
        let ld = (light.GetDirectionFromPoint hitPoint)     // Light direction

        // Determine the colour
        if n * ld > 0. then
            let diffuse = (kd * cd) / Math.PI
            let volume = (light.GetGeometricFactor hitPoint / light.GetProbabilityDensity hitPoint)
            let roundness = lc * (n * ld)
            let matte = diffuse * volume * roundness   
            matte
        else
            Colour.Black


//- SPECULAR REFLECTION MATERIAL (PHONG)     
type PhongMaterial 
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        specularColour: Colour, 
        specularCoefficient: float, 
        specularExponent: int
    ) = 
    inherit MatteMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient)
    
    member this.Super = (this :> MatteMaterial)
    member this.SpecularCoefficient = specularCoefficient
    member this.SpecularColour = specularColour
    member this.SpecularExponent = specularExponent
    
    default this.Bounce(shape, hitPoint, light) = 
        
        // Initialize parameters
        let ld = (light.GetDirectionFromPoint hitPoint).Normalise   // Light direction
        let n = hitPoint.Normal                                     // Normal at hit point
        let r1 = -ld + (2. * (n * ld)) * n                          // Light ray direction
        let ray = hitPoint.Ray                                      // Casted ray
        let rd = ray.GetDirection                                   // Direction of casted ray
        let e = specularExponent                                    // Specular exponent
        let ks = specularCoefficient                                // Specular coefficient
        let cs = specularColour                                     // Specular colour
        let lc  = light.GetColour hitPoint                          // Light colour
        
        // Detemine the colour
        if n * ld > 0. then

            // The standard diffuse colour
            let matte = base.Bounce(shape, hitPoint, light) 
            
            // The specular colour
            let specular = 
                if r1 * -rd > 0. then
                    ks * cs * ((r1 * (-rd)) ** float(e))
                else
                    Colour.Black
            let direction = lc * (n * ld)
            
            // The final colour
            (matte + specular) * direction
        else
            Colour.Black


//- MATTE REFLECTIVE MATERIAL
type MatteReflectiveMaterial 
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        reflectionColour: Colour, 
        reflectionCoefficient: float
    ) = 
    inherit MatteMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient)
    
    member this.Super = (this :> MatteMaterial)
    member this.ReflectionColour = reflectionColour            
    member this.ReflectionCoefficient = reflectionCoefficient   
    default this.ReflectionFactor = reflectionColour * reflectionCoefficient
    default this.IsRecursive = true
    default this.BounceMethod hitPoint = 
        // Determine the perfect outgoing ray
        let dir = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal
        let rayDirection = (dir + (-2. * (normal * dir)) * normal)

        // Only one reflected ray
        [| Ray(hitPoint.EscapedPoint, rayDirection) |]

    default this.Bounce(shape, hitPoint, light) = 
        // Bounce the diffuse material, handle the reflection in the raycaster
        base.Bounce(shape, hitPoint, light)


//- MATTE GLOSSY REFLECTIVE MATERIAL
type MatteGlossyReflectiveMaterial     
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        reflectiveColour: Colour,
        glossyCoefficient: float,
        glossyExponent: int,
        sampler: Sampler
    ) = 
    inherit MatteMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient)
    
    member this.Super = (this :> MatteMaterial)
    default this.ReflectionFactor = reflectiveColour * glossyCoefficient
    default this.IsRecursive = true
    default this.BounceMethod hitPoint =
        
        // Prepare for sampling
        let direction = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal
        let rays = Array.create sampler.SampleCount Ray.None

        // Sample the outgoing rays
        for i=0 to sampler.SampleCount-1 do
            
            // Get a sample point from the sampler, and map it to a hemisphere
            let sp = Tracer.Basics.Point(mapToHemisphere (sampler.Next()) (float(glossyExponent)))

            // Reflected ray direction
            let m = direction + 2. * (normal * -direction) * normal

            // Transform orthonormal frame of sample point
            let up = new Vector(0., 1., 0.)
            let w = m.Normalise
            let v = (up % w).Normalise
            let u = w % v
            let transformed_sp = sp.OrthonormalTransform (u, v, w)

            // Add outgoing ray to the array
            rays.[i] <- 
                if transformed_sp * normal > 0. then
                    Ray(hitPoint.Point, transformed_sp)
                else
                    Ray(hitPoint.Point, -sp.X * u - sp.Y * v + sp.Z * w)
        
        // Return the rays to be handler in the raycaster
        rays

    default this.Bounce(shape, hitPoint, light) = 
        // Bounce the diffuse material
        base.Bounce(shape, hitPoint, light)


//- PHONG REFLECTIVE MATERIAL
type PhongReflectiveMaterial 
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        specularColour: Colour, 
        specularCoefficient: float, 
        reflectionColour: Colour, 
        reflectionCoefficient: float,
        specularExponent: int
    ) = 
    inherit PhongMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient, specularColour, specularCoefficient, specularExponent)
    
    member this.Super = (this :> PhongMaterial)
    member this.ReflectionColour = reflectionColour            
    member this.ReflectionCoefficient = reflectionCoefficient   
       
    default this.ReflectionFactor = reflectionColour * reflectionCoefficient
    default this.IsRecursive = true
    default this.BounceMethod hitPoint = 
        // Determine the perfect outgoing ray
        let dir = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal
        let rayDirection = (dir + (-2. * (normal * dir)) * normal)

        // Only one reflected ray
        [| Ray(hitPoint.EscapedPoint, rayDirection) |]

    default this.Bounce(shape, hitPoint, light) = 
        // Bounce the diffuse material, handle the reflection in the raycaster
        base.Bounce(shape, hitPoint, light)


//- MATTE GLOSSY REFLECTIVE MATERIAL
type PhongGlossyReflectiveMaterial     
    (
        ambientColour: Colour, 
        ambientCoefficient: float, 
        matteColour: Colour, 
        matteCoefficient: float,
        specularColour: Colour, 
        specularCoefficient: float, 
        reflectiveColour: Colour,
        glossyCoefficient: float,
        specularExponent: int,
        glossyExponent: int,
        sampler: Sampler
    ) = 
    inherit PhongMaterial(ambientColour, ambientCoefficient, matteColour, matteCoefficient, specularColour, specularCoefficient, specularExponent)
    
    member this.ReflectiveColour = reflectiveColour
    member this.GlossyCoefficient = glossyCoefficient
    member this.Super = (this :> PhongMaterial)
    default this.ReflectionFactor = reflectiveColour * glossyCoefficient
    default this.IsRecursive = true
    default this.BounceMethod hitPoint =
        
        // Prepare for sampling
        let direction = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal
        let rays = Array.create sampler.SampleCount Ray.None

        // Sample the outgoing rays
        for i=0 to sampler.SampleCount-1 do
            
            // Get a sample point from the sampler, and map it to a hemisphere
            let sp = Tracer.Basics.Point(mapToHemisphere (sampler.Next()) (float(glossyExponent)))

            // Reflected ray direction
            let m = direction + 2. * (normal * -direction) * normal

            // Transform orthonormal frame of sample point
            let up = new Vector(0., 1., 0.)
            let w = m.Normalise
            let v = (up % w).Normalise
            let u = w % v
            let transformed_sp = sp.OrthonormalTransform (u, v, w)

            // Add outgoing ray to the array
            rays.[i] <- 
                if transformed_sp * normal > 0. then
                    Ray(hitPoint.Point, transformed_sp)
                else
                    Ray(hitPoint.Point, -sp.X * u - sp.Y * v + sp.Z * w)
        
        // Return the rays to be handler in the raycaster
        rays

    default this.Bounce(shape, hitPoint, light) = 
        // Bounce the diffuse material
        base.Bounce(shape, hitPoint, light)


//- EMISSIVE MATERIAL
type EmissiveMaterial
    (
        lightColour: Colour, 
        lightIntensity: float
    ) = 
    inherit Material()

    let emisiveRadience = lightColour * lightIntensity
    member this.LightColour = lightColour
    member this.LightIntensity = lightIntensity
    member this.EmisiveRadience = emisiveRadience
    default this.AmbientColour = Colour.Black
    default this.IsRecursive = false
    default this.ReflectionFactor = Colour.White
    default this.BounceMethod hitPoint = [||]
    default this.Bounce(shape, hitPoint, light) = 
        
        // Only emit light from the front
        if hitPoint.Normal * -hitPoint.Ray.GetDirection > 0. then emisiveRadience
        else Colour.Black

type TransparentMaterial
    (
        innerFilterColour: Colour, 
        outerFilterColour: Colour, 
        innerRefractionIndex: float, 
        outerRefractionIndex: float
    ) = 
    inherit Material()

    let refrIndex = (innerRefractionIndex / outerRefractionIndex)

    default this.IsRecursive = true
    default this.ReflectionFactor = Colour.White
    default this.AmbientColour = Colour.Black
    default this.BounceMethod hitPoint = 
        // Determine the perfect outgoing ray
        let dir = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal
        let rayDirection = (dir + (-2. * (normal * dir)) * normal)

        // Only one reflected ray
        [| Ray(hitPoint.EscapedPoint, rayDirection) |]
    default this.Bounce(shape, hitPoint, light) =
        Colour.Black

    member this.ShouldRefract (hitPoint: HitPoint) = 
        let cos_angle_in = hitPoint.Normal * -hitPoint.Ray.GetDirection
        let angle_in = Math.Acos(cos_angle_in)
        let cos_angle_out_exp = 1. - (1. - cos(angle_in) ** 2.) / (refrIndex ** 2.)
        if cos_angle_out_exp >= 0. then
            (true, cos_angle_in, cos_angle_out_exp)
        else
            (false, cos_angle_in, cos_angle_out_exp)
    member this.RefractRay (hitPoint: HitPoint) (cos_angle_in, cos_angle_out_exp) = 
        let cos_angle_out = Math.Sqrt(cos_angle_out_exp)
        let origin = hitPoint.InnerEscapedPoint
        let direction = ((1.) / (refrIndex)) * hitPoint.Ray.GetDirection - (cos_angle_out - (cos_angle_in) / (refrIndex)) * hitPoint.Normal
        Ray(origin, direction)