namespace Tracer.Basics
open System
open Tracer.Sampling
open System.Numerics

exception LightException

//- MATERIAL
[<AbstractClass>]
type Material() = 
    abstract member Bounce: Shape -> HitPoint -> Light -> Colour
    abstract member AmbientColour: Shape -> HitPoint -> Colour
    abstract member Bounces: int
    abstract member BounceMethod: HitPoint -> Ray[]
    member this.IsRecursive: bool = this.Bounces > 0
    member this.PreBounce (shape: Shape) (hitPoint: HitPoint) (light: Light) = 
        if light :? AmbientLight then
            this.AmbientColour shape hitPoint * light.Intensity
        else
            this.Bounce shape hitPoint light
    static member None = BlankMaterial()

and BlankMaterial() = 
    inherit Material()
    default this.Bounce shape hitPoint light = Colour.Black
    default this.AmbientColour shape hitPoint = Colour.Black
    default this.Bounces = 0
    default this.BounceMethod hitPoint = [| hitPoint.Ray |]
      
//- HITPOINT
and HitPoint(ray: Ray, time: float, normal: Vector, material: Material, didHit: bool) = 
    
    member this.Ray: Ray = ray
    member this.Time: float = time
    member this.Point: Point = ray.PointAtTime time
    member this.EscapedPoint: Point = (ray.PointAtTime time) + this.Normal * 0.000001
    member this.DidHit: bool = didHit
    member this.Normal: Vector = if ray.GetDirection * normal > 0. then -normal else normal
    member this.Material: Material = material
    
    // For hit rays
    new(ray: Ray, time:float, normal: Vector, material: Material) = 
        HitPoint(ray, time, normal, material, true)

    // For missed rays
    new(ray: Ray) = HitPoint(ray, 0., new Vector(0.,0.,0.), Material.None, false)
    new(point: Point) = HitPoint(Ray.None, 0., point.ToVector, Material.None, false)

//- LIGHT
and [<AbstractClass>] Light(colour: Colour, intensity: float) =
    let colour = colour
    let intensity = intensity
    member this.BaseColour: Colour = colour
    member this.Intensity: float = intensity

    // (l_c) Final colour
    abstract member GetColour: HitPoint -> Colour

    // (l_d) Direction from a point to this light
    abstract member GetDirectionFromPoint: HitPoint -> Vector

    // (_ls) Shadow ray
    abstract member GetShadowRay: HitPoint -> Ray[]

    // (l_G) Geometric factor
    abstract member GetGeometricFactor: HitPoint -> float

    // (l_pdf) Probability density function
    abstract member GetProbabilityDensity: HitPoint -> float

and AmbientLight(colour: Colour, intensity: float) =
    inherit Light(colour, intensity)

    override this.GetColour hitPoint = 
        new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint hitPoint = 
        raise LightException
    override this.GetShadowRay hitPoint = 
        raise LightException
    override this.GetGeometricFactor hitPoint = 
        1.
    override this.GetProbabilityDensity hitPoint = 
        1.

//- SHAPE
and [<AbstractClass>] Shape() =
    abstract member isInside: Point -> bool
    abstract member getBoundingBox: unit -> BBox
    abstract member hitFunction: Ray -> HitPoint
    static member None = BlankShape() :> Shape

and BlankShape() = 
    inherit Shape()
    override this.isInside (p:Point) = failwith "cannot be inside a blank shape"
    override this.getBoundingBox () = failwith "cannot get bounding box for a blank shape"
    default this.hitFunction r = HitPoint(r)

//- TEXTURES
and Texture =
    | Texture of (float -> float -> Material)