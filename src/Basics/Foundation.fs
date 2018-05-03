namespace Tracer.Basics
open System
open Tracer.Sampling

exception LightException

//- MATERIAL
[<AbstractClass>]
type Material() = 
    abstract member Bounce: Shape -> HitPoint -> Light -> Colour
    abstract member AmbientColour: Shape -> HitPoint -> Colour
    abstract member Bounces: int
    abstract member BounceMethod: HitPoint -> Ray[]
    member this.IsRecursive = this.Bounces > 0
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
    
    member this.Ray = ray
    member this.Time = time
    member this.Point: Point = ray.PointAtTime time
    member this.DidHit = didHit
    member this.Normal = normal
    member this.Material = material
    
    // For hit rays
    new(ray: Ray, time:float, normal: Vector, material: Material) = 
        HitPoint(ray, time, normal, material, true)

    // For missed rays
    new(ray: Ray) = HitPoint(ray, 0., new Vector(0.,0.,0.), Material.None, false)

//- LIGHT
and [<AbstractClass>] Light(colour: Colour, intensity: float) =
    let colour = colour
    let intensity = intensity
    member this.BaseColour = colour
    member this.Intensity: float = intensity

    // (l_c) Final colour
    abstract member GetColour: Point -> Colour

    // (l_d) Direction from a point to this light
    abstract member GetDirectionFromPoint: Point -> Vector

    // (_ls) Shadow ray
    abstract member GetShadowRay: HitPoint -> Ray[]

    // (l_G) Geometric factor
    abstract member GetGeometricFactor: Point -> float

    // (l_pdf) Probability density function
    abstract member GetProbabilityDensity: float

and AmbientLight(colour: Colour, intensity: float) =
    inherit Light(colour, intensity)

    override this.GetColour point = 
        new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint (point:Point) = 
        raise LightException
    override this.GetShadowRay (hitPoint:HitPoint) = 
        raise LightException
    override this.GetGeometricFactor point = 
        1.
    override this.GetProbabilityDensity = 
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