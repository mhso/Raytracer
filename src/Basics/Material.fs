namespace Tracer.Basics
open System

[<AbstractClass>]
type Material() = 
    abstract member Bounce: Point*Vector -> Ray -> Light -> Colour

//- MATTE MATERIAL
type MatteMaterial(colour:Colour) = 
    inherit Material()

    let colour = colour
    let coefficient = 1.
    member this.Colour = colour
    member this.Coefficient = coefficient
    
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
