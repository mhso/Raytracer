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
    abstract member BounceMethod: HitPoint -> Ray[]
    abstract member IsRecursive : bool
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
    default this.BounceMethod hitPoint = [| hitPoint.Ray |]
    default this.IsRecursive = false
      
//- HITPOINT
and HitPoint(ray: Ray, time: float, normal: Vector, material: Material, shape: Shape, u: float, v:float, didHit: bool) = 
    
    member this.Ray = ray
    member this.Time = time
    member this.Point: Point = ray.PointAtTime time
    member this.EscapedHitpoint: Point = (ray.PointAtTime time) + normal
    member this.DidHit = didHit
    member this.Normal = normal
    member this.Material = material
    member this.U = u
    member this.V = v
    member this.UV = (u,v)
    member this.Shape = shape

    override this.Equals(other) =
        match other with
        | :? HitPoint as h ->
                                if (this.Ray.Equals(h.Ray)
                                    && this.Time.Equals(h.Time)
                                    && this.Point.Equals(h.Point)
                                    && this.Normal.Equals(h.Normal)
                                    && this.Material.Equals(h.Material)) then true
                                 else false
        | _ -> false
    member this.GetHashCode = 
        hash (this.Ray, this.Time, this.Point, this.DidHit, this.Normal, this.Material)
    // For hit rays
    new(ray: Ray, time:float, normal: Vector, material: Material, shape: Shape) = 
        HitPoint(ray, time, normal, material, shape, 0., 0., true)

    new(ray: Ray, time:float, normal:Vector, material:Material, shape:Shape, u:float, v:float) = 
        HitPoint(ray, time, normal, material, shape, u, v, true)

    // For missed rays
    new(ray: Ray) = 
        HitPoint(ray, 0., new Vector(0.,0.,0.), Material.None, Shape.None, 0., 0., false)

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