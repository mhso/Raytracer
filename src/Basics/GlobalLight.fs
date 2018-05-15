namespace Tracer.Basics
open Tracer.Basics.Sampling
open System

//- ENVIRONMENT LIGHT
type EnvironmentLight(radius: float, texture: Texture, sampler: Sampler) = 
    inherit Light(Colour.Black, 1.)
    
    let sphere = SphereShape(Point.Zero, radius, texture)
    let mutable sps = []
    let mutable ld = Vector.Zero
    let mutable lpdf = 1.
    member this.FlushDirections (hitPoint: HitPoint) = 
        let generateSample() = 
            let (x,y,z) = Sampling.mapToHemisphere (sampler.Next()) 0.
            let sp_local = Point(x/2.,y/2.,z)
            let up = new Vector(0., 1., 0.)
            let w = hitPoint.Normal
            let v = (up % w).Normalise
            let u = w % v
            sp_local.OrthonormalTransform(u, v, w)
        sps <- [for i in 1..sampler.SampleCount do yield generateSample()]
        ld <- sps |> List.average
        lpdf <- [for sp in sps do yield (sp * hitPoint.Normal) / Math.PI] |> List.average

    member this.Radius = radius
    member this.Texture = texture
    member this.Sampler = sampler
    member this.Sphere = sphere

    
    // Handled in Render.fs, for environment lights only
    // Below functions are unused
    override this.GetGeometricFactor hitPoint = 1. 
    override this.GetColour hitPoint = 
        let getColour sp = 
            let ray = Ray(hitPoint.EscapedPoint, sp)
            let hit = sphere.hitFunction(ray)
            hit.Material.Bounce(sphere, hit, this)
        [for sp in sps do yield getColour sp] |> List.average

    override this.GetDirectionFromPoint hitPoint = 
        hitPoint.Normal

    override this.GetShadowRay (hitPoint:HitPoint) =
        if hitPoint.Material :? EmissiveMaterial then
            [||]
        else
            let getRay sp = 
                Ray(hitPoint.EscapedPoint + sp * 0.5, sp)
            [for sp in sps do yield getRay sp] |> List.toArray
    override this.GetProbabilityDensity hitPoint = 
        lpdf

//- AMBIENT OCCLUDER
type AmbientOccluder (intensity: float, c: Colour, min_intensity: float, s: Sampler) = 
    inherit AmbientLight(c, intensity)
    member this.Intensity = intensity
    member this.MinIntensity = min_intensity
    member this.MinIntensityColour = min_intensity * c * intensity
    member this.Colour = c * intensity
    member this.Sampler = s