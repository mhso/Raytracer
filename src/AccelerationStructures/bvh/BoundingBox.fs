namespace Tracer

type BoundingBox (boundingbox:(Point * Point), axis:int) =

    // private fields
    let boundingbox = boundingbox
    let axis = axis

    // public fields
    member this.getBoundingBox = boundingbox
    member this.getAxis = axis