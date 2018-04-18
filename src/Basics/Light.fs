namespace Tracer.Basics

[<AbstractClass>]
type Light(colour: Colour, intensity: float) =
    let colour = colour
    let intensity = intensity
    member this.BaseColour = colour
    member this.Intensity = intensity
    member this.GetColour = new Colour(colour.R * this.Intensity, colour.G * intensity, colour.B * intensity)
    abstract member GetDirectionFromPoint: Point -> Vector
    abstract member GetShadowRay: Point -> Ray

exception AmbientLightException
type AmbientLight(colour: Colour, intensity: float) =
    inherit Light(colour, intensity)
    override this.GetDirectionFromPoint (point:Point) = 
        raise AmbientLightException
    override this.GetShadowRay (hitPoint:Point) = 
        raise AmbientLightException

type PointLight(colour: Colour, intensity: float, position: Point) = 
    inherit Light(colour, intensity)
    let position = position
    member this.Position = position
    override this.GetDirectionFromPoint (hitPoint:Point) = 
        - (hitPoint - position)
    override this.GetShadowRay (hitPoint:Point) = 
        new Ray((hitPoint),(this.GetDirectionFromPoint hitPoint).Invert)
    
type DirectionalLight(colour: Colour, intensity: float, direction: Vector) = 
    inherit Light(colour, intensity)
    let direction = direction
    member this.Direction = direction
    override this.GetDirectionFromPoint (hitPoint:Point) = 
        direction
    override this.GetShadowRay (hitPoint:Point) = 
        new Ray((hitPoint),(this.GetDirectionFromPoint hitPoint).Invert)


        