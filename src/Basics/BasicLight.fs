namespace Tracer.Basics

//- POINT LIGHT
type PointLight(colour: Colour, intensity: float, position: Point) = 
    inherit Light(colour, intensity)

    member this.Position = position

    override this.GetColour point = 
        new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint hitPoint = 
        (position - hitPoint.Point).Normalise
    override this.GetShadowRay hitPoint = 
        let normal = hitPoint.Normal
        let shadowRayOrigin = hitPoint.Point + normal * 0.00001
        let direction = (position - shadowRayOrigin).Normalise
        [| new Ray(shadowRayOrigin, direction) |]
    override this.GetGeometricFactor point = 
        1.
    override this.GetProbabilityDensity = 
        1.


//- DIRECTIONAL LIGHT  
type DirectionalLight(colour: Colour, intensity: float, direction: Vector) = 
    inherit Light(colour, intensity)

    member this.Direction = direction

    override this.GetColour point = 
        new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint hitPoint = 
        direction.Normalise
    override this.GetShadowRay (hitPoint:HitPoint) =
        let shadowRayOrigin = hitPoint.Point + hitPoint.Normal * 0.00001
        [| new Ray(shadowRayOrigin, direction.Normalise) |]
    override this.GetGeometricFactor point = 
        1.
    override this.GetProbabilityDensity = 
        1.