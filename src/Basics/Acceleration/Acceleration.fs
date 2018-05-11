namespace Tracer.Basics

module Acceleration = 
    open KD_tree
    open BVH
    open RegularGrids

    let mutable acceleration = ""

    type IAcceleration = KDTree of KDTree
                       | BVHStructure of BVHStructure
                       | RGStructure of RGStructure

    let createAcceleration (shapes:array<Shape>) = 
        match acceleration with
        | "KDTree" -> KDTree(buildKDTree shapes)
        | "BVH"    -> BVHStructure (BVH.build shapes)
        | "RG"     -> RGStructure (RegularGrids.build shapes)
        | _        -> KDTree(buildKDTree shapes) //Default...

    let getAccelBoundingBox (accel:IAcceleration) = 
        match accel with
        | KDTree(kdTree) -> 
            match kdTree with
            | KDTree.Leaf(bBox, shapes) -> bBox
            | KDTree.Node(axis, value, bBox, left, right) -> bBox
        | BVHStructure(bvh)       -> failwith "Not Implemented"
        | RGStructure(rg)         -> failwith "Not Implemented"

    let traverseIAcceleration (accel:IAcceleration) (ray:Ray) (shapes:array<Shape>) = 
        match accel with
        | KDTree(kdTree) -> traverseKDTree kdTree ray shapes
        | BVHStructure(bvhStructure) -> BVH.traverse bvhStructure ray shapes
        | RGStructure(rgStructure) -> RegularGrids.traverse rgStructure ray shapes

    type Acceleration = KDTree
                      | BVH
                      | RegularGrid

    let setAcceleration (accel : Acceleration) : unit = 
        match accel with
        | KDTree -> acceleration <- "KDTree"
        | BVH -> acceleration <- "BVH"
        | RegularGrid -> acceleration <- "RG"