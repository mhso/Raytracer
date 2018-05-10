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
    let getBaseTexturedMaterial(texture: Texture) = {
            new Material() with
                member this.AmbientColour = Colour.Black
                member this.ReflectionFactor = Colour.White
                member this.Bounce(shape, hitPoint, light) = 
                    let func = getFunc texture
                    let mat = func hitPoint.U hitPoint.V
                    mat.Bounce(shape,hitPoint,light)
                member this.BounceMethod hitPoint = [| hitPoint.Ray |]
                member this.IsRecursive = false}
        