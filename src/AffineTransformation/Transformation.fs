module Transformation
open System.Windows
open System
open Tracer.Basics
open System

    type Transformation = 
        | T of float list list
    let mkTransformation (a: float list list) = T(a)
    let getRowLength (T(a)) = a.Length //Gets the number of rows
    let getColLength (T(a)) = a.Head.Length //Get the number of columns in a matrix

    let transpose (T(a)) = //Transpose a given matrix
        let rec trans l = 
            match l with
            | row::rows ->
                match row with
                | col::cols -> 
                    let firstElement = List.map List.head l 
                    let additional = trans (List.map List.tail l)
                    firstElement::additional
                | _ -> []
            | _ -> []  
        mkTransformation(trans a)   

    type Transformation with
        static member multi ((T(a)),(T(b))) = 
            let rec multiplyList ((a:float list),(b:float list)) = 
                match a,b with 
                | [x],[y] -> x*y
                | currentU::restU, currentV::restV -> currentU*currentV + multiplyList (restU,restV)
                | _ -> 1.

            let multiply ((T(c)),(T(d))) = c |> List.map 
                                                (fun row -> d |> List.map 
                                                                                (fun col -> multiplyList (row,col)))
            mkTransformation(multiply ((T(a)),(transpose(T(b)))))
    end

    let identityMatrixWithPos x y z = mkTransformation ([[1.0;0.0;0.;x];[0.;1.;0.;y];[0.;0.;1.;z];[0.;0.;0.;1.]]) //CREATES AN IDENTITY MATRIX
    let getList (T(a)) = a
    let vectorToMatrix (v:Vector) = mkTransformation ([[1.0;0.0;0.;v.X];[0.;1.;0.;v.Y];[0.;0.;1.;v.Z];[0.;0.;0.;0.]])
    let pointToMatrix (p:Point) = identityMatrixWithPos p.X p.Y p.Z

    let translate x y z = identityMatrixWithPos x y z
    let translateInv x y z = translate -x -y -z

    let scale width height depth = mkTransformation ([[width;0.0;0.;0.];[0.;height;0.;0.];[0.;0.;depth;0.];[0.;0.;0.;1.]])
    let scaleInv width height depth = scale -width -height -depth
    let mirrorX = scale -1. 1. 1.
    let mirrorY = scale 1. -1. 1.
    let mirrorZ = scale 1. 1. -1.
    
    let sheareXY dist = mkTransformation ([[1.;0.;0.;0.];[dist;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    let sheareXZ dist = mkTransformation ([[1.;0.;0.;0.];[0.;1.;0.;0.];[dist;0.;1.;0.];[0.;0.;0.;1.]])
    let sheareYX dist = mkTransformation ([[1.;dist;0.;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    let sheareYZ dist = mkTransformation ([[1.;0.;0.;0.];[0.;1.;0.;0.];[0.;dist;1.;0.];[0.;0.;0.;1.]])
    let sheareZX dist = mkTransformation ([[1.;0.;dist;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    let sheareZY dist = mkTransformation ([[1.;0.;0.;0.];[0.;1.;dist;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])

    let rotateX angle = mkTransformation ([[1.;0.;0.;0.];[0.;Math.Cos(angle);-(Math.Sin(angle));0.];[0.;Math.Sin(angle);-(Math.Cos(angle));0.];[0.;0.;0.;1.]])
    let rotateXInv angle = mkTransformation ([[1.;0.;0.;0.];[0.;Math.Cos(angle);(Math.Sin(angle));0.];[0.;-(Math.Sin(angle));-(Math.Cos(angle));0.];[0.;0.;0.;1.]])
    let rotateY angle = mkTransformation ([[Math.Cos(angle);0.;Math.Sin(angle);0.];[0.;1.;0.;0.];[-(Math.Sin(angle));0.;Math.Cos(angle);0.];[0.;0.;0.;1.]])
    let rotateYInv angle = mkTransformation ([[Math.Cos(angle);0.;-(Math.Sin(angle));0.];[0.;1.;0.;0.];[(Math.Sin(angle));0.;Math.Cos(angle);0.];[0.;0.;0.;1.]])
    let rotateZ angle = mkTransformation ([[Math.Cos(angle);-(Math.Sin(angle));0.;0.];[Math.Sin(angle);Math.Cos(angle);0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    let rotateZInv angle = mkTransformation ([[Math.Cos(angle);(Math.Sin(angle));0.;0.];[-(Math.Sin(angle));Math.Cos(angle);0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])

    let mergeTransformations (l: Transformation List) = 
        let rec sum (value,l2)= 
            match l2 with
            | first::rest ->
                let v = Transformation.multi(value,first)
                sum(v,rest)
            | _ -> value
        let emptyTrans = mkTransformation ([[1.];[1.];[1.];[1.]])
        sum (emptyTrans,l)

    let transform = failwith("NOT IMPLEMENTED")

    let matrixToVector (T(a)) = 
        let x = a.Head.Item(a.Head.Length-1)
        let y = a.Item(1).Item(a.Item(1).Length-1)
        let z = a.Item(2).Item(a.Item(2).Length-1)
        new Vector(x, y, z)

    let matrixToPoint (T(a)) = 
        let x = a.Head.Item(a.Head.Length-1)
        let y = a.Item(1).Item(a.Item(1).Length-1)
        let z = a.Item(2).Item(a.Item(2).Length-1)
        new Point(x, y, z)

    let transformDirectionalLight ((light:DirectionalLight),t) = 
        let matrix = vectorToMatrix (light.GetDirectionFromPoint (new Point(0.,0.,0.)))
        let transMatrix = Transformation.multi (t,matrix)
        matrixToVector transMatrix

    let transformPointLight ((light:PointLight),t) = 
        let matrix = pointToMatrix (light.Position)
        let transMatrix = Transformation.multi (t,matrix)
        matrixToPoint transMatrix

    let transformLight (light:Light) t =
        match light with
        | :? DirectionalLight as d -> DirectionalLight(d.BaseColour, d.Intensity, transformDirectionalLight (d,t)) :> Light
        | :? PointLight as p -> PointLight(p.BaseColour, p.Intensity, transformPointLight (p,t)) :> Light
        | _ -> light