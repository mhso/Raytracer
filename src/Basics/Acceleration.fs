namespace Tracer.Basics

module Acceleration = 
    open KD_tree

    type Acceleration = KDTree of KDTree
                      | BVH of float // Swap with actual BVH
                      | RG of float // Swap with actual RG

    let setAcceleration (accel:Acceleration) (ray:Ray) (shapes:array<Shape>) = 
        match accel with
        | KDTree(kdTree) -> traverseKDTree kdTree ray shapes
        | BVH(bvh) -> failwith "Not Implemented"
        | RG(rg) -> failwith "Not Implemented"