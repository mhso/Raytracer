namespace Tracer.Basics

module Acceleration = 
    open KD_tree
    open Tracer.BVH

    let mutable acceleration = ""

    type IAcceleration = KDTree of KDTree
                       | BVHStructure of BVHStructure
                       | RegularGrid of float // Swap with actual RG

    let createAcceleration (shapes:array<Shape>) = 
        match acceleration with
        | "KDTree" -> KDTree(buildKDTree shapes)
        | "BVH"    -> BVHStructure (build shapes)
        | "RG"     -> failwith "Not Implemented"
        | _        -> KDTree(buildKDTree shapes) //Default...

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