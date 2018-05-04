﻿namespace Tracer.Basics
open System
open Tracer.Sampling
open System.Drawing

//- MATTE MATERIAL
type MatteMaterial(colour:Colour) = 
    inherit Material()

    let colour = colour
    let coefficient = 1.
    
    default this.Bounces = 0
    default this.BounceMethod hitPoint = [||]
    member this.Colour = colour
    member this.Coefficient = coefficient
    default this.AmbientColour shape hitPoint = colour
    
    default this.Bounce (shape: Shape) (hitPoint: HitPoint) (light: Light) = 
        
        // Initialize parameters 
        let kd  = coefficient                           // Matte coefficient
        let cd  = colour                                // Matte colour
        let lc:Colour  = light.GetColour hitPoint // Light colour
        let n   = hitPoint.Normal                       // Normal at hit point
        let ld  = (light.GetDirectionFromPoint hitPoint)  // Light direction

        // Determine the colour
        if n * ld > 0. then
            let friction    = (kd * cd) / Math.PI     
            let volume      = (light.GetGeometricFactor hitPoint / light.GetProbabilityDensity)
            let direction   = lc * (n * ld)                 
            friction * volume * direction
        else
            Colour.Black

//- SPECULAR REFLECTION MATERIAL (PHONG)     
type SpecularMaterial 
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
    default this.BounceMethod hitPoint = [||]
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
            let l:Vector = (light.GetDirectionFromPoint hitPoint)
            l.Normalise // Light direction
        let n = hitPoint.Normal                         // Normal at hit point
        let r1 = -ld + (2. * (n * ld)) * n             // Light ray direction
        let ray:Ray = hitPoint.Ray
        let rd = ray.GetDirection             // Direction of ray
        let e = specularExponent                       // Specular exponent
        let ks = specularCoefficient                   // Specular coefficient
        let cs = specularColour                        // Specular colour
        let lc  = light.GetColour hitPoint       // Light colour
        
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

type PerfectReflectionMaterial(bounces: int, baseMaterial: Material, reflectionColour: Colour, reflectionCoefficient: float) =
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

type GlossyMaterial(reflectionCoefficient: float, reflectionColour: Colour, baseMaterial: Material, sampleCount: int, setCount: int, bounces: int, sharpness: float) = 
    inherit Material()
    
    let random = new Random()
    let samplingGenerator = new Sampling.SampleGenerator(Sampling.multiJittered, sampleCount, setCount)

    // Will reflect a ray along a hemisphere
    default this.Bounces = bounces
    default this.BounceMethod hitPoint =
        let direction = hitPoint.Ray.GetDirection
        let normal = hitPoint.Normal
        let rays = Array.create sampleCount Ray.None

        for i = 0 to sampleCount-1 do
            let sp = Tracer.Basics.Point(Sampling.mapToHemisphere (samplingGenerator.Next()) sharpness)
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

type EmissiveMaterial(lightColour: Colour, lightIntensity: float) = 
    inherit Material()

    let emisiveRadience = lightColour * lightIntensity

    member this.LightColour = lightColour
    member this.LightIntensity = lightIntensity
    member this.EmisiveRadience = emisiveRadience
    default this.Bounces = 0
    default this.BounceMethod hitPoint = [||]
    default this.AmbientColour shape hitPoint = emisiveRadience
    default this.Bounce (shape: Shape) (hitPoint: HitPoint) (light: Light) = 
        emisiveRadience

type TexturedMaterial (uvFunc: (float * float) -> (float * float), image: Bitmap) = 
    inherit Material()

    let round (x:float) = int (Math.Round(x))

    default this.Bounces = 0
    default this.BounceMethod hitPoint = [||]
    default this.AmbientColour shape hitPoint = 
        let (u,v) = shape.getTextureCoords hitPoint |> uvFunc
        let (x,y) = (round (u * float(image.Width)), round (v * float(image.Height)))
        Colour(image.GetPixel(x,y))
    default this.Bounce shape hitPoint light = 
        light.Intensity * this.AmbientColour shape hitPoint
