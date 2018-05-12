namespace Tracer.Basics
open System

module Textures = 

    let mkTexture func = 
        let funcWrapper u v = 
            let uN = if Double.IsNaN(u) then 0. else u
            let vN = if Double.IsNaN(v) then 0. else v
            let uF = if uN < 0. then 1. - abs(uN) % 1. else uN % 1.
            let vF = if vN < 0. then 1. - abs(vN) % 1. else vN % 1.
            func uF vF
        Texture funcWrapper

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
        