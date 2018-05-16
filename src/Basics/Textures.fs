namespace Tracer.Basics
open System

module Textures = 

    let mkTexture func =
        Texture func

    let mkMatTexture mat =
        let func _ _ = mat
        Texture func

    let getFunc (Texture func) = func
    
    //- For use with material-only data structures
    let getBaseTexturedMaterial(texture: Texture) = {
            new Material() with
                member this.AmbientColour(hitPoint, ambientLight) = Colour.Black
                member this.ReflectionFactor (hitPoint, rayOut) = Colour.White
                member this.Bounce(shape, hitPoint, light, ambientLight) = 
                    let func = getFunc texture
                    let mat = func hitPoint.U hitPoint.V
                    mat.Bounce(shape,hitPoint,light,ambientLight)
                member this.BounceMethod hitPoint = [| hitPoint.Ray |]
                member this.IsRecursive = false}
        