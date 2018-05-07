﻿namespace Tracer.Basics
open Tracer.Sampling
open System

[<AbstractClass>]
type AreaLight(surfaceMaterial: EmissiveMaterial, sampleCount: int, sampleSetCount: int) = 
    inherit Light(surfaceMaterial.LightColour, surfaceMaterial.LightIntensity)

    override this.GetColour hitPoint = 
        let point = hitPoint.Point
        let x = 
            [for i in [0..sampleCount] do 
                let sp = this.SamplePoint point
                let n = this.SamplePointNormal sp
                yield 
                    if n * (point - sp).Normalise > 0. then
                        surfaceMaterial.EmisiveRadience
                    else
                        Colour.Black] 
           
        let total = x |> List.fold (fun acc c -> acc + c) Colour.Black
        total / float(sampleCount)

    override this.GetDirectionFromPoint hitPoint = 
        let point = hitPoint.Point
        let total = [for i=0 to sampleCount - 1 do yield (this.SamplePoint point - point).Normalise] |> List.sum
        total / float(sampleCount)

    override this.GetShadowRay hitPoint = 
        if hitPoint.Material :? EmissiveMaterial then
            [||]
        else
            let shadowRays = Array.create sampleCount Ray.None
            for i=0 to sampleCount-1 do 
                let sp = this.SamplePoint hitPoint.Point
                let shadowRayOrigin = hitPoint.Point + hitPoint.Normal * 0.00001
                let direction = (sp - shadowRayOrigin).Normalise
                shadowRays.[i] <- Ray(shadowRayOrigin, direction)
            shadowRays

    override this.GetGeometricFactor hitPoint = 
        let p = hitPoint.Point
        let sp = this.SamplePoint p
        let sp_n = this.SamplePointNormal sp
        (sp_n * (p - sp).Normalise) / ((p - sp) * (p - sp))

    override this.GetProbabilityDensity hitPoint = 
        this.Density
    
    member this.SurfaceMaterial = surfaceMaterial
    member this.SampleCount = sampleCount
    member this.SampleSetCount = sampleSetCount

    abstract member FlushSample: unit -> unit
    abstract member SamplePoint: Point -> Point
    abstract member SamplePointNormal: Point -> Vector
    abstract member Density: float

type DiscAreaLight(surfaceMaterial: EmissiveMaterial, disc: Disc, sampleCount: int, sampleSetCount: int) = 
    inherit AreaLight (surfaceMaterial, sampleCount, sampleSetCount)

    let sampleGenerator = Sampling.SampleGenerator(Sampling.multiJittered, sampleCount, sampleSetCount)

    override this.SamplePoint point = 
        let sp = sampleGenerator.Next()
        let (x,y) = Sampling.mapToDisc sp
        Point(x * disc.radius, y * disc.radius, disc.center.Z)
    override this.SamplePointNormal point = 
        disc.normal
    override this.Density = 
        Math.PI * disc.radius * disc.radius
    override this.FlushSample() = 
        ignore sampleGenerator.Next

type RectangleAreaLight(surfaceMaterial: EmissiveMaterial, rect: Rectangle, sampleCount: int, sampleSetCount: int) = 
    inherit AreaLight (surfaceMaterial, sampleCount, sampleSetCount)

    let sampleGenerator = Sampling.SampleGenerator(Sampling.multiJittered, sampleCount, sampleSetCount)

    override this.SamplePoint point = 
        let (x,y) = sampleGenerator.Next()
        Point(x * rect.width, y * rect.height, 0.)
    override this.SamplePointNormal point = 
        rect.normal
    override this.Density = 
        rect.width * rect.height
    override this.FlushSample() = 
        ignore sampleGenerator.Next

type SphereAreaLight(surfaceMaterial: EmissiveMaterial, sphere: SphereShape, sampleCount: int, sampleSetCount: int) = 
    inherit AreaLight (surfaceMaterial, sampleCount, sampleSetCount)

    let sampleGenerator = Sampling.SampleGenerator(Sampling.multiJittered, sampleCount, sampleSetCount)

    override this.SamplePoint point = 
        let hem_sp = Point((Sampling.mapToHemisphere (sampleGenerator.Next()) 50.))
        let d_c_p = (point - sphere.Origin).Normalise
        let up = new Vector(0., 1., 0.)
        let w = d_c_p.Normalise
        let v = (up % w).Normalise
        let u = w % v
        let v = hem_sp.OrthonormalTransform (u, v, w) 
        Point(sphere.Origin.X + (v.X / 2.), sphere.Origin.Y + (v.Y / 2.), sphere.Origin.Z + (v.Z / 2.))

    override this.SamplePointNormal point = 
        (point - sphere.Origin).Normalise
    override this.Density = 
        2. * Math.PI * (sphere.Radius * sphere.Radius)
    override this.FlushSample() = 
        ignore sampleGenerator.Next
        
type EnvironmentLight(radius: float, texture: Texture, sampler: Sampling.SampleGenerator) = 
    inherit Light(Colour.Black, 1.)
    
    let sphere = SphereShape(Point.Zero, radius, texture)
    let mapSample (x, y, z) = (x, z, y)
    let samples = [for i=1 to sampler.SampleCount do yield Point((Sampling.mapToHemisphere (sampler.Next()) 1.) |> mapSample) * radius]
    let getSampleColour sp = 
        let hitPoint = sphere.hitFunction(Ray(Point.Zero, sp))
        if hitPoint.Material :? EmissiveMaterial then
            (hitPoint.Material :?> EmissiveMaterial).EmisiveRadience
        else
            Colour.Black
    let colour = [for sp in samples do yield getSampleColour sp.ToVector] |> List.average

    member this.Radius = radius
    member this.Texture = texture
    member this.Sampler = sampler

    override this.GetColour hitPoint = 
        if hitPoint.Point.ToVector.Magnitude < radius then 
            colour
        else 
            printfn "no"
            Colour.Black
        
    override this.GetDirectionFromPoint hitPoint = 
        hitPoint.Normal

    override this.GetShadowRay (hitPoint:HitPoint) =
        if hitPoint.Material :? EmissiveMaterial then 
            [||]
        else
            [for sp in samples do yield Ray(hitPoint.EscapedPoint, (sp - hitPoint.Point).Normalise)] |> List.toArray
                
    override this.GetGeometricFactor hitPoint =
        1.

    override this.GetProbabilityDensity hitPoint =
        1.
    
