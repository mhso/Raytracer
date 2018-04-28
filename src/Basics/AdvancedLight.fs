namespace Tracer.Basics
open Tracer.Sampling
open System

[<AbstractClass>]
type AreaLight(surfaceMaterial: EmissiveMaterial, sampleCount: int, sampleSetCount: int) = 
    inherit Light(surfaceMaterial.LightColour, surfaceMaterial.LightIntensity)

    override this.GetColour point = 
        if this.SamplePointNormal point * (point - this.SamplePoint point).Normalise > 0. then
            surfaceMaterial.EmisiveRadience
        else
            surfaceMaterial.EmisiveRadience

    override this.GetDirectionFromPoint (point: Point) = 
        let total = [for i=0 to sampleCount - 1 do yield (this.SamplePoint point - point).Normalise] |> List.sum
        total / float(sampleCount)
    override this.GetShadowRay (hitPoint: HitPoint) = 
        let shadowRays = Array.create sampleCount Ray.None
        for i=0 to sampleCount - 1 do 
            let sp = this.SamplePoint hitPoint.Point
            printfn "%A" sp
            let normal:Vector = hitPoint.Normal
            let shadowRayOrigin = sp + normal * 0.00001
            let direction = (sp - shadowRayOrigin).Normalise
            shadowRays.[i] <- Ray(shadowRayOrigin, direction)
        shadowRays

    override this.GetGeometricFactor (point: Point) = 
        let d_sp_p = (point - this.SamplePoint point)
        let d_sp_p_norm = d_sp_p.Normalise
        let sp_n = this.SamplePointNormal point
        (sp_n * d_sp_p_norm) / (d_sp_p * d_sp_p)

    override this.GetProbabilityDensity = 
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

    let samplingAlgorithm sampleCount sampleSetCount = 
        Sampling.multiJittered sampleCount sampleSetCount
        |> Array.map(fun s -> Sampling.mapToDisc s)

    let sampleGenerator = Sampling.SampleGenerator(samplingAlgorithm, sampleCount, sampleSetCount)
    do ignore sampleGenerator.Next

    override this.SamplePoint point = 
        let (x,y) = sampleGenerator.Current
        let sp = Point(disc.center.X + x, disc.center.Y + y, disc.center.Z)
        Point(sp.X * disc.radius, sp.Y * disc.radius, sp.Z)
    override this.SamplePointNormal point = 
        disc.normal
    override this.Density = 
        Math.PI * disc.radius * disc.radius
    override this.FlushSample() = 
        ignore sampleGenerator.Next

type RectangleAreaLight(surfaceMaterial: EmissiveMaterial, rect: Rectangle, sampleCount: int, sampleSetCount: int) = 
    inherit AreaLight (surfaceMaterial, sampleCount, sampleSetCount)

    let sampleGenerator = Sampling.SampleGenerator(Sampling.multiJittered, sampleCount, sampleSetCount)
    do ignore sampleGenerator.Next

    override this.SamplePoint point = 
        let (x,y) = sampleGenerator.Current
        let sp = Point(rect.bottomleft.X + x, rect.bottomleft.Y + y, rect.bottomleft.Z)
        Point(sp.X * rect.width, sp.Y * rect.height, sp.Z)
    override this.SamplePointNormal point = 
        rect.normal
    override this.Density = 
        rect.width * rect.height
    override this.FlushSample() = 
        ignore sampleGenerator.Next

type SphereAreaLight(surfaceMaterial: EmissiveMaterial, sphere: SphereShape, sampleCount: int, sampleSetCount: int) = 
    inherit AreaLight (surfaceMaterial, sampleCount, sampleSetCount)

    let sampleGenerator = Sampling.HemisphereSampleGenerator(sampleCount, sampleSetCount)

    override this.SamplePoint point = 
        let hem_sp = Point(sampleGenerator.Next())
        let d_c_p = (point - sphere.Origin).Normalise
        let up = new Vector(0., 1., 0.)
        let w = d_c_p.Normalise
        let v = (up % w).Normalise
        let u = w % v
        let v = hem_sp.OrthonormalTransform (u, v, w)
        Point(v.X, v.Y, v.Z)

    override this.SamplePointNormal point = 
        (point - sphere.Origin).Normalise
    override this.Density = 
        2. * Math.PI * sphere.Radius * sphere.Radius
    override this.FlushSample() = 
        ignore sampleGenerator.Next
