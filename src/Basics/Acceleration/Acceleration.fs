namespace Tracer.Basics

module Acceleration = 
    open KD_tree

    let mutable acceleration = ""

    type IAcceleration = KDTree of KDTree
                       | BVH of float // Swap with actual BVH
                       | RG of float // Swap with actual RG

    let createAcceleration (shapes:array<Shape>) = 
        match acceleration with
        | "KDTree" -> KDTree(buildKDTree shapes)
        | "BVH"    -> failwith "Not Implemented"
        | "RG"     -> failwith "Not Implemented"
        | _        -> failwith "Unknown Acceleration Structure..."

    let getAccelBoundingBox (accel:IAcceleration) = 
        match accel with
        | KDTree(kdTree) -> kdTree.bBox
        | BVH(bvh)       -> failwith "Not Implemented"
        | RG(rg)         -> failwith "Not Implemented"

    let traverseIAcceleration (accel:IAcceleration) (ray:Ray) (shapes:array<Shape>) = 
        match accel with
        | KDTree(kdTree) -> traverseKDTree kdTree ray shapes
        | BVH(bvh) -> failwith "Not Implemented" //Look above...
        | RG(rg) -> failwith "Not Implemented" //Look above...


    type Acceleration = KDTree
                      | BVH
                      | RG

    let setAcceleration (accel : Acceleration) : unit = 
        match accel with
        | KDTree -> acceleration <- "KDTree"
        | BVH -> acceleration <- "BVH"
        | RG -> acceleration <- "RG"