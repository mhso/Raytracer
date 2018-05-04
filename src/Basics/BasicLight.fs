namespace Tracer.Basics

//- POINT LIGHT
type PointLight(colour: Colour, intensity: float, position: Point) = 
    inherit Light(colour, intensity)

    member this.Position = position

    override this.GetColour point = 
        new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint (point:Point) = 
        (position - point).Normalise
    override this.GetShadowRay (hitPoint:HitPoint) = 
        let normal:Vector = hitPoint.Normal
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
    override this.GetDirectionFromPoint (point:Point) = 
        direction.Normalise
    override this.GetShadowRay (hitPoint:HitPoint) =
        let shadowRayOrigin = hitPoint.Point + hitPoint.Normal * 0.00001
        [| new Ray(shadowRayOrigin, direction.Normalise) |]
    override this.GetGeometricFactor point = 
        1.
    override this.GetProbabilityDensity = 
        1.

module TransformLight = 
    let transformDirectionalLight ((light:DirectionalLight),t) = 
        let matrix = Transformation.vectorToMatrix (light.GetDirectionFromPoint (new Point(0.,0.,0.)))
        let transMatrix = Transformation.Matrix.multi (Transformation.getMatrix(t),matrix)
        Transformation.matrixToVector transMatrix

    let transformPointLight ((light:PointLight),t) = 
        let matrix = Transformation.pointToMatrix (light.Position)
        let transMatrix = Transformation.Matrix.multi (Transformation.getMatrix(t),matrix)
        Transformation.matrixToPoint transMatrix

    let transformLight (light:Light) t =
        match light with
        | :? DirectionalLight as d -> DirectionalLight(d.BaseColour, d.Intensity, transformDirectionalLight (d,t)) :> Light
        | :? PointLight as p -> PointLight(p.BaseColour, p.Intensity, transformPointLight (p,t)) :> Light
        | _ -> light