namespace Tracer.Basics
open System

[<AbstractClass>]
type Material() = 
    abstract member Bounce: Point*Vector -> Ray -> Light -> Colour
    abstract member AmbientColour: Colour
    member this.PreBounce (hitPoint: Point, normal: Vector) (ray: Ray) (light: Light) = 
        if light :? AmbientLight then
            light.GetColour * this.AmbientColour
        else
            this.Bounce (hitPoint, normal) ray light

//- MATTE MATERIAL
type MatteMaterial(colour:Colour) = 
    inherit Material()

    let colour = colour
    let coefficient = 1.
    
    member this.Colour = colour
    member this.Coefficient = coefficient
    default this.AmbientColour = colour
    
    default this.Bounce (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        // Initialize parameters 
        let kd  = coefficient                           // Matte coefficient
        let cd  = colour                                // Matte colour
        let lc  = light.GetColour                       // Light colour
        let n   = normalHitPoint                        // Normal at hit point
        let ld  = (light.GetDirectionFromPoint hitPoint).Normalise  // Light direction

        // Determine the colour
        if n * ld > 0. then
            let friction    = (kd * cd) / Math.PI           
            let direction   = lc * (n * ld)                 
            friction * direction
        else
            Colour.Black

//- SPECULAR REFLECTION MATERIAL (PHONG)     
type SpecularMaterial 
    (
        specularCoefficient: float, 
        specularColour: Colour, 
        specularExponent: float, 
        matteColour: Colour
    ) = 
    
    inherit MatteMaterial(matteColour)
    
    let specularCoefficient = 1.
    let specularColour = specularColour
    let matteColour = matteColour
    let matteMaterial = new MatteMaterial(matteColour)
    default this.AmbientColour = matteColour
    member this.SpecularCoefficient = specularCoefficient
    member this.SpecularColour = specularColour
    member this.MatteColour = matteColour
    member this.MatteMaterial = matteMaterial
    
    default this.Bounce (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        
        // Initialize parameters
        let kd = matteMaterial.Coefficient             // Matte coefficient
        let cd = matteMaterial.Colour                  // Matte colour
        let ld  = (light.GetDirectionFromPoint hitPoint).Normalise // Light direction
        let n = normalHitPoint                         // Normal at hit point
        let r1 = -ld + (2. * (n * ld)) * n             // Light ray direction
        let rd = ray.GetDirection.Normalise            // Direction of ray
        let e = specularExponent                       // Specular exponent
        let ks = specularCoefficient                   // Specular coefficient
        let cs = specularColour                        // Specular colour
        let lc  = light.GetColour                      // Light colour
        
        if n * ld > 0. then
            let matteFriction = matteMaterial.Bounce (hitPoint, normalHitPoint) ray light
            let specularFriction = 
                if r1 * -rd > 0. then
                    ks * cs * ((r1 * (-rd)) ** e)
                else
                    Colour.Black
            let direction = lc * (n * ld)
            
            (matteFriction + specularFriction) * direction
        else
            Colour.Black

//- SPECULAR REFLECTION MATERIAL (BLINN-PHONG)     
type BlinnPhongMaterial 
    (
        specularCoefficient: float, 
        specularColour: Colour, 
        specularExponent: float, 
        matteColour: Colour
    ) = 
    
    inherit SpecularMaterial(specularCoefficient, specularColour, specularExponent, matteColour)
    
    default this.Bounce (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        let kd = this.MatteMaterial.Bounce (hitPoint, normalHitPoint) ray light
        let L = light.GetDirectionFromPoint hitPoint
        let V = new Vector(4., 0., 0.) - hitPoint
        let H = (L + V).Normalise
        let Is = Math.Pow(Math.Max(0.0, (H * normalHitPoint)), specularExponent)
        let diffuse = kd
        let specular = new Colour(1.0,1.0,1.0) * Is * specularCoefficient
        diffuse + specular

//- PERFECT REFLECTION MATERIALS
type PerfectReflectionMaterial(bounces: int, baseMaterial: Material, specularCoefficient: float) =
    inherit Material()
    member this.BaseMaterial = baseMaterial
    member this.SpecularCoefficient = specularCoefficient
    default this.AmbientColour = Colour.White
    default this.Bounce (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        baseMaterial.Bounce (hitPoint,normalHitPoint) ray light
    member this.Bounces = bounces

type MixedMaterial(a: Material, b: Material, factor: float) =
    inherit Material()
    member this.MaterialA = a
    member this.MaterialB = b
    member this.Factor = factor
    default this.AmbientColour = Colour.White
    default this.Bounce (hitPoint: Point, normalHitPoint: Vector) (ray: Ray) (light: Light) = 
        let colorA = a.Bounce (hitPoint, normalHitPoint) ray light
        let colorB = b.Bounce (hitPoint, normalHitPoint) ray light
        colorA.Scale(1.-factor) + colorB.Scale(factor)
        
     