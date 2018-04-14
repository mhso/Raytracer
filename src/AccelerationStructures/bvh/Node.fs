namespace Tracer

type Node (boundingbox, axis, leftChild:Node option, rightChild:Node option) =
    inherit BoundingBox(boundingbox, axis)

    // private fields
    let leftChild = leftChild
    let rightChild = rightChild

    // public fields
    member this.getLeftChild = leftChild
    member this.getLeft = rightChild
