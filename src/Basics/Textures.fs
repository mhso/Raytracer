namespace Tracer.Basics
open System.Drawing

module Textures = 

    let mkTexture func = 
        Texture func

    let mkMatTexture mat =
        let func x y = mat
        Texture func

    let getFunc (Texture func) = func