﻿namespace Tracer.Basics

open Tracer.Sampling.Sampling
open System
open Tracer.BaseShape

//- AREA LIGHT
[<AbstractClass>]
type AreaLight(surfaceMaterial: EmissiveMaterial, sampler: Sampler) = 
    inherit Light(surfaceMaterial.LightColour, surfaceMaterial.LightIntensity)

    member this.Texture = Textures.mkMatTexture surfaceMaterial // Texture of emissive surface
    member this.SurfaceMaterial = surfaceMaterial               // Material of emissive surface
    member this.SampleCount = sampler.SampleCount               // Sampler sample count
    member this.SampleSetCount = sampler.SetCount               // Sampler sample set count
    abstract member Shape: Shape                                // Shape of the surface
    abstract member SamplePoint: Point -> Point                 // Returns a new sample point
    abstract member SamplePointNormal: Point -> Vector          // Returns the normal of a sample point

    // Colour of the light
    override this.GetColour hitPoint = 
        // List the colours of the samples
        let totalColourList = 
            [for i in [0..sampler.SampleCount] do 
                let sp = this.SamplePoint hitPoint.Point
                let n = this.SamplePointNormal sp
                yield 
                    if n * (hitPoint.Point - sp).Normalise > 0. then
                        surfaceMaterial.EmisiveRadience
                    else
                        Colour.Black] 
        
        // Return the average
        totalColourList |> List.average
        
    // Hit direction
    override this.GetDirectionFromPoint hitPoint = 
        // Get the average direction for the samples
        [for i=0 to sampler.SampleCount-1 do yield (this.SamplePoint hitPoint.Point - hitPoint.Point).Normalise] |> List.average

    // Shadow rays
    override this.GetShadowRay hitPoint = 
        // EmissiveMaterial does not cast or recieve shadows
        if hitPoint.Material :? EmissiveMaterial then [||]
        else
            // Generate shadow rays for the samples
            let shadowRays = Array.create sampler.SampleCount Ray.None
            for i=0 to sampler.SampleCount-1 do 
                let sp = this.SamplePoint hitPoint.Point
                let shadowRayOrigin = hitPoint.Point + hitPoint.Normal * 0.00001
                let direction = (sp - shadowRayOrigin).Normalise
                shadowRays.[i] <- Ray(shadowRayOrigin, direction)
            shadowRays
    
    // Geometric factor
    override this.GetGeometricFactor hitPoint = 
        let p = hitPoint.Point
        let sp = this.SamplePoint p
        let sp_n = this.SamplePointNormal sp
        (sp_n * (p - sp).Normalise) / ((p - sp) * (p - sp))
    

//- DISC (AREA LIGHT)
type DiscAreaLight(surfaceMaterial: EmissiveMaterial, disc: BaseDisc, sampler: Sampler) = 
    inherit AreaLight (surfaceMaterial, sampler)
    
    override this.Shape = disc.toShape this.Texture

    override this.SamplePoint point = 
        let sp = sampler.Next()
        let (x,y) = mapToDisc sp
        Point(x * disc.radius, y * disc.radius, disc.center.Z)

    override this.SamplePointNormal point = 
        Vector(0.,1.,0.)

    override this.GetProbabilityDensity hitPoint = 
        Math.PI * disc.radius * disc.radius


//- RECTANGLE (AREA LIGHT)
type RectangleAreaLight(surfaceMaterial: EmissiveMaterial, rect: BaseRectangle, sampler: Sampler) = 
    inherit AreaLight (surfaceMaterial, sampler)
    
    override this.Shape = rect.toShape this.Texture
    member this.RectShape = this.Shape :?> Rectangle

    override this.SamplePoint point = 
        let (x,y) = sampler.Next()
        Point(x * rect.width, y * rect.height, 0.)

    override this.SamplePointNormal point = 
        this.RectShape.normal

    override this.GetProbabilityDensity hitPoint = 
        rect.width * rect.height


//- SPHERE (AREA LIGHT)
type SphereAreaLight(surfaceMaterial: EmissiveMaterial, sphere: BaseShape, sampler: Sampler) = 
    inherit AreaLight (surfaceMaterial, sampler)
    
    override this.Shape = sphere.toShape this.Texture
    member this.SphereShape = this.Shape :?> SphereShape
    override this.SamplePoint point = 
        // Hemisphere transform the samples
        let hem_sp = Point((mapToHemisphere (sampler.Next()) 10.))

        // Transform orthonormal coordinate space
        let d_c_p = (point - this.SphereShape.Origin).Normalise
        let up = new Vector(0., 1., 0.)
        let w = d_c_p.Normalise
        let v = (up % w).Normalise
        let u = w % v
        let v = hem_sp.OrthonormalTransform (u, v, w)

        // Return the sample point
        Point(v.X/2.,v.Y/2., v.Z/2.)

    override this.SamplePointNormal point = 
        (point - this.SphereShape.Origin).Normalise

    override this.GetProbabilityDensity hitPoint = 
        2. * Math.PI * (this.SphereShape.Radius * this.SphereShape.Radius)