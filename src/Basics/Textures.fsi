namespace Tracer.Basics

module Textures =
    val mkTexture : (float -> float -> Material) -> Texture
    val mkMatTexture : Material -> Texture
    val mkTextureFromFile : (float * float -> float * float) -> string -> Texture
    val getFunc : Texture -> (float -> float -> Material)