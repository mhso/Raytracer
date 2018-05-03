namespace Tracer.Basics

module Texture =
    val mkTexture : (float -> float -> Material) -> Texture
    val mkMatTexture : Material -> Texture
    val mkTextureFromFile : (float * float -> float * float) -> string -> Texture