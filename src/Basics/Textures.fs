namespace Tracer.Basics
open System.Drawing

module Texture = 

    let mkTexture func = 
        Texture func

    let mkMatTexture mat =
        let func x y = mat
        Texture func

    let mkTextureFromFile uvFunc (filePath: string) =
        let image = new Bitmap(filePath)
        let mat = TexturedMaterial(uvFunc, image) :> Material
        let func x y = mat
        Texture func

    let getFunc (Texture func) = func

    let bounce (texture:Texture) (shape:Shape) (hitPoint:HitPoint) (light:Light) = 
        let func = getFunc texture
        let (u,v) = shape.getTextureCoords hitPoint
        let mat = func u v
        mat.Bounce shape hitPoint light