﻿namespace Tracer.Basics

module Acceleration = 
    open KD_tree
    open BVH
    open RegularGrids
    
    let mutable acceleration = "KDTree"

    type IAcceleration = | KDTree of KDTree
                         | BVHStructure of BVHStructure
                         | RGStructure of RGStructure
                         member this.IsEmpty = false

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
                accel
            | "BVH"    ->
                let accel = BVHStructure (BVH.build shapes)
                listOfKDTree <- (shapeArray(shape.number,shape.shapes,Some accel))::listOfKDTree
                accel
            | "RG"     -> 
                let accel =  RGStructure (RegularGrids.build shapes)
                listOfKDTree <- (shapeArray(shape.number,shape.shapes,Some accel))::listOfKDTree
                accel
            | _        -> failwith "NOT A ACCELERATION TYPE"
        else listOfKDTree.[shape.number-1].acceleration.Value
        

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