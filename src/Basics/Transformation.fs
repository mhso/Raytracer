module Tracer.Basics.Transformation

open System.Windows
open System
open Tracer.Basics
open System

    type Matrix = 
        | M of float array array
    type Transformation = 
        | T of Matrix * Matrix
    let mkMatrix (a: float array array) = 
        match a.Length,a.[0].Length with
        | 4,4 -> M(a)
        | _ -> failwith ("not correct size matrix")
    let mkTransformation (a,b) = T(a,b)
    let getRowLength (M(a)) = a.Length //Gets the number of rows
    let getColLength (M(a)) = a.[0].Length //Get the number of columns in a matrix

    let transpose (M(a)) = //Transpose a given matrix7
        let c = Array.zeroCreate(4)
        for i in 0..3 do
            let b = Array.zeroCreate(4)
            for j in 0..3 do
                b.[j] <- a.[j].[i]
            c.[i] <- b
        mkMatrix (c) 

    type Matrix with
        static member multi ((M(a)),(M(b))) = 
            let outer = Array.zeroCreate(4)
            for i in 0..3 do
                let inner = Array.zeroCreate(4)
                for j in 0..3 do
                    let mutable value = 0.
                    for k in 0..3 do 
                        value <- a.[i].[k] * b.[k].[j] + value
                    inner.[j] <- value
                outer.[i] <- inner
            mkMatrix (outer)
    end

    let identityMatrixWithPos x y z = mkMatrix ([|[|1.0;0.0;0.;x|];[|0.;1.;0.;y|];[|0.;0.;1.;z|];[|0.;0.;0.;1.|]|]) //CREATES AN IDENTITY MATRIX
    let getList (M(a)) = a
    let getMatrix (T(a,_)) = a
    let getInvMatrix (T(_,b)) = b
    let vectorToMatrix (v:Vector) = mkMatrix ([|[|1.0;0.0;0.;v.X|];[|0.;1.;0.;v.Y|];[|0.;0.;1.;v.Z|];[|0.;0.;0.;0.|]|])
    let pointToMatrix (p:Point) = identityMatrixWithPos p.X p.Y p.Z

    let translate x y z = mkTransformation (identityMatrixWithPos x y z, identityMatrixWithPos -x -y -z)

    let scale width height depth = mkTransformation (mkMatrix ([|[|width;0.0;0.;0.|];[|0.;height;0.;0.|];[|0.;0.;depth;0.|];[|0.;0.;0.;1.|]|]),
                                                     mkMatrix ([|[|Math.Pow(width,-1.);0.0;0.;0.|];[|0.;Math.Pow(height,-1.);0.;0.|];[|0.;0.;Math.Pow(depth,-1.);0.|];[|0.;0.;0.;1.|]|]))
    let sheare (xy,xz,yx,yz,zx,zy) = 
        let matrix = mkMatrix([|[|1.;yx;zx;0.|];[|xy;1.;zy;0.|];[|xz;yz;1.;0.|];[|0.;0.;0.;1.|]|])
        let det = (1.-(xy*yx)+(xz*zx)-(yz*zy)+(xy*yz*zx)+(xz*yz*zy))
        let mult = 1./det
        //TODO: Ask what is wrong with the inverse
        let inv = 
            mkMatrix (
                [|[|mult*(1.-(yz*zy));mult*(-yx+yz*zx);mult*(-zx+yx*zy);0.|];
                [|mult*(-xy+xz*zy);mult*(1.-xz*zx);mult*(-zy+xy*zx);0.|];
                [|mult*(-xz+xy*yz);mult*(-yz+xz*yx);mult*(1.-xy*yx);0.|];
                [|0.;0.;0.;mult*det|]|])
        mkTransformation(matrix,inv)

    let rotateX angle = mkTransformation(mkMatrix ([|[|1.;0.;0.;0.|];[|0.;Math.Cos(angle);-(Math.Sin(angle));0.|];[|0.;Math.Sin(angle);(Math.Cos(angle));0.|];[|0.;0.;0.;1.|]|]),
                                         mkMatrix ([|[|1.;0.;0.;0.|];[|0.;Math.Cos(angle);(Math.Sin(angle));0.|];[|0.;-(Math.Sin(angle));(Math.Cos(angle));0.|];[|0.;0.;0.;1.|]|]))
    let rotateY angle = mkTransformation(mkMatrix ([|[|Math.Cos(angle);0.;Math.Sin(angle);0.|];[|0.;1.;0.;0.|];[|-(Math.Sin(angle));0.;Math.Cos(angle);0.|];[|0.;0.;0.;1.|]|]),
                                         mkMatrix ([|[|Math.Cos(angle);0.;-(Math.Sin(angle));0.|];[|0.;1.;0.;0.|];[|(Math.Sin(angle));0.;Math.Cos(angle);0.|];[|0.;0.;0.;1.|]|]))
    let rotateZ angle = mkTransformation(mkMatrix ([|[|Math.Cos(angle);-(Math.Sin(angle));0.;0.|];[|Math.Sin(angle);Math.Cos(angle);0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]),
                                          mkMatrix ([|[|Math.Cos(angle);(Math.Sin(angle));0.;0.|];[|-(Math.Sin(angle));Math.Cos(angle);0.;0.|];[|0.;0.;1.;0.|];[|0.;0.;0.;1.|]|]))
                                          


    let mergeMatrix (l : Matrix List) = 
        let rec sum (value,l2)= 
            match l2 with
            | first::rest ->
                let v = Matrix.multi(value,first)
                sum(v,rest)
            | _ -> value
        sum (l.Head,l.Tail)

    let mergeTransformations (l: Transformation List) : Transformation = 
        let matrixList = l |> List.map (fun a -> getMatrix a)
        let NewMatrix = mergeMatrix matrixList
        let reverseMatrixList = (List.rev l) |> List.map (fun a -> getInvMatrix a)
        let newInverseMatrix = mergeMatrix reverseMatrixList
        mkTransformation (NewMatrix, newInverseMatrix)

    let matrixToVector (M(a)) = 
        let x = a.[0].[3]
        let y = a.[1].[3]
        let z = a.[2].[3]
        new Vector(x, y, z)

    let matrixToPoint (M(a)) = 
        let x = a.[0].[3]
        let y = a.[1].[3]
        let z = a.[2].[3]
        new Point(x, y, z)


    
