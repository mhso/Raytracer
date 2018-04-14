namespace Tracer

type Leaf (boundingbox, axis, shape:Shape) =
    inherit BoundingBox(boundingbox, axis)
    // private fields
    let shape = shape

    // public fields
    member this.getShape = shape
