namespace Tracer.Basics
open System
open Tracer.Basics.Sampling
open System.Numerics

exception LightException

//- MATERIAL
[<AbstractClass>]
type Material() = 
    abstract member Bounce: Shape * HitPoint * Light * AmbientLight -> Colour
    abstract member BounceMethod: HitPoint -> Ray[]
    abstract member IsRecursive : bool
    abstract member ReflectionFactor : HitPoint * Ray -> Colour
    abstract member AmbientColour : HitPoint * AmbientLight -> Colour
    member this.PreBounce (shape: Shape, hitPoint: HitPoint, light: Light, ambientLight: AmbientLight) =
        this.Bounce(shape,hitPoint,light, ambientLight)
    static member None = BlankMaterial()

and BlankMaterial() = 
    inherit Material()
    default this.AmbientColour(hitPoint, ambientLight) = Colour.Black
    default this.ReflectionFactor(hitPoint,rayOut) = Colour.White
    default this.Bounce(shape, hitPoint, light, ambientLight) = Colour.Black
    default this.BounceMethod hitPoint = [| hitPoint.Ray |]
    default this.IsRecursive = false
      
//- HITPOINT
and HitPoint(ray: Ray, time: float, normal: Vector, material: Material, shape: Shape, u: float, v:float, didHit: bool) = 
    
    member this.Ray: Ray = ray
    member this.Time: float = time
    member this.Point: Point = ray.PointAtTime time
    member this.EscapedPoint: Point = this.Point + normal * 0.000001
    member this.InnerEscapedPoint: Point = this.Point - normal * 0.000001
    member this.DidHit = didHit
    member this.Normal = if ray.GetDirection * normal > 0. then -normal else normal
    member this.Material = material
    member this.U = u
    member this.V = v
    member this.UV = (u,v)
    member this.Shape = shape

    // For hit rays
    new(ray: Ray, time:float, normal: Vector, material: Material, shape: Shape) = 
        HitPoint(ray, time, normal, material, shape, 0., 0., true)

    new(ray: Ray, time:float, normal:Vector, material:Material, shape:Shape, u:float, v:float) = 
        HitPoint(ray, time, normal, material, shape, u, v, true)

    // For missed rays
    new(ray: Ray) = 
        HitPoint(ray, -0., new Vector(0.,0.,0.), Material.None, Shape.None, 0., 0., false)
    new(point: Point) = 
        HitPoint(Ray.None, -0., point.ToVector, Material.None, Shape.None, 0., 0., false)

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

    default this.GetColour hitPoint = 
        colour * intensity
    override this.GetDirectionFromPoint hitPoint = 
        hitPoint.Normal
    override this.GetShadowRay hitPoint = 
        [||]
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