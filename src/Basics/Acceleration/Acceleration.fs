namespace Tracer.Basics

module Acceleration = 
    open KD_tree
    open Tracer.BVH

    let mutable acceleration = "KDTree"

    type IAcceleration = KDTree of KDTree
                       | BVHStructure of BVHStructure
                       | RegularGrid of float // Swap with actual RG

    type shapeArray (number: int, shapes:Shape array, acceleration:IAcceleration Option)  = 
        member this.number = number
        member this.shapes = shapes
        member this.acceleration = acceleration

    let mutable listOfKDTree : shapeArray list = []

    let createAcceleration (shape: shapeArray) = 
        if (listOfKDTree.Length < shape.number) then
            let shapes = shape.shapes
            match acceleration with
            | "KDTree" -> 
                let accel = KDTree(buildKDTree shapes)
                listOfKDTree <- (shapeArray(shape.number,shape.shapes,Some accel))::listOfKDTree
                printfn "Number of kdtrees %A" shape.number
                accel
            | "BVH"    ->
                let accel = BVHStructure (build shapes)
                listOfKDTree <- (shapeArray(shape.number,shape.shapes,Some accel))::listOfKDTree
                accel
            | "RG"     -> failwith "Not Implemented"
            | _        -> failwith "NOT A ACCELERATION TYPE"
        else listOfKDTree.[shape.number-1].acceleration.Value

    let getAccelBoundingBox (accel:IAcceleration) = 
        match accel with
        | KDTree(kdTree) -> 
            match kdTree with
            | KDTree.Leaf(bBox, shapes) -> bBox
            | KDTree.Node(axis, value, bBox, left, right) -> bBox
        | BVHStructure(bvh)       -> failwith "Not Implemented"
        | RegularGrid(rg)         -> failwith "Not Implemented"

    let traverseIAcceleration (accel:IAcceleration) (ray:Ray) (shapes:array<Shape>) = 
        match accel with
        | KDTree(kdTree) -> traverseKDTree kdTree ray shapes
        | BVHStructure(bvhStructure) -> traverse bvhStructure ray shapes
        | RegularGrid(rg) -> failwith "Not Implemented" //Look above...

    type Acceleration = KDTree
                      | BVH
                      | RegularGrid

    let setAcceleration (accel : Acceleration) : unit = 
        match accel with
        | KDTree -> acceleration <- "KDTree"
        | BVH -> acceleration <- "BVH"
        | RegularGrid -> acceleration <- "RG"