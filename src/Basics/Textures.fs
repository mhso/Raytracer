namespace Tracer.Basics
open System.Drawing

module Textures = 

    let mkTexture func = 
        Texture func

    let mkMatTexture mat =
        let func x y = mat
        Texture func

    let getFunc (Texture func) = func
    
    //- For use with material-only data structures
    type TexturedMaterial(texture: Texture) = 
        inherit Material()
        default this.AmbientColour = Colour.Black
        default this.ReflectionFactor = Colour.White
        default this.Bounce(shape, hitPoint, light) = 
            let func = getFunc texture
            let mat = func hitPoint.U hitPoint.V
            mat.Bounce(shape,hitPoint,light)
        default this.BounceMethod hitPoint = [| hitPoint.Ray |]
        default this.IsRecursive = false