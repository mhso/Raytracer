namespace Tracer.Basics
open System.Drawing

module Textures = 

    let mkTexture func = 
        Texture func

    let mkMatTexture mat =
        let func x y = mat
        Texture func

        (*
    let mkTextureFromFile uvFunc (filePath: string) =
        let image = new Bitmap(filePath)
        let mat = TexturedMaterial(uvFunc, image) :> Material
        let func x y = mat
        Texture func
        *)
    let getFunc (Texture func) = func