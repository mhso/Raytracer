namespace Tracer.Basics
open Tracer.Basics.Sampling

//- ENVIRONMENT LIGHT
type EnvironmentLight(radius: float, texture: Texture, sampler: Sampler) = 
    inherit Light(Colour.Black, 1.)
    
    let sphere = SphereShape(Point.Zero, radius, texture)
    let mapSample (x, y, z) = (x, z, y)
    let samples = [for i=1 to sampler.SampleCount do yield Point((mapToHemisphere (sampler.Next()) 1.) |> mapSample) * radius]
    let getSampleColour (sp:Vector) = 
        let hitPoint = sphere.hitFunction(Ray(Point.Zero, sp.Normalise))
        if hitPoint.Material :? EmissiveMaterial then
            (hitPoint.Material :?> EmissiveMaterial).EmisiveRadience
        else
            Colour.Black
    let colour = [for sp in samples do yield getSampleColour sp.ToVector] |> List.average

    member this.Radius = radius
    member this.Texture = texture
    member this.Sampler = sampler

    override this.GetColour hitPoint = 
        if hitPoint.Point.ToVector.Magnitude < radius then colour
        else Colour.Black

    override this.GetDirectionFromPoint hitPoint = 
        hitPoint.Normal

    override this.GetShadowRay (hitPoint:HitPoint) =
        if hitPoint.Material :? EmissiveMaterial then [||]
        else [for sp in samples do yield Ray(hitPoint.EscapedPoint, (sp - hitPoint.Point).Normalise)] |> List.toArray
                
    override this.GetGeometricFactor hitPoint = 1. 
    override this.GetProbabilityDensity hitPoint = 1.
    
//- AMBIENT OCCLUDER
type AmbientOccluder (intensity: float, c: Colour, min_intensity: float, s: Sampler) = 
    inherit AmbientLight(c, intensity)
    member this.Intensity = intensity
    member this.MinIntensity = min_intensity
    member this.MinIntensityColour = min_intensity * c * intensity
    member this.Colour = c * intensity
    member this.Sampler = s