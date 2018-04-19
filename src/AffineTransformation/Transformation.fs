module Transformation
open System.Windows
open System
open Tracer.Basics
open System

    type Matrix = 
        | M of float list list
    type Transformation = 
        | T of Matrix * Matrix
    let mkMatrix (a: float list list) = M(a)
    let mkTransformation (a,b) = T(a,b)
    let getRowLength (M(a)) = a.Length //Gets the number of rows
    let getColLength (M(a)) = a.Head.Length //Get the number of columns in a matrix

    let transpose (M(a)) = //Transpose a given matrix
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
        mkMatrix(trans a)   

    type Matrix with
        static member multi ((M(a)),(M(b))) = 
            let rec multiplyList ((a:float list),(b:float list)) = 
                match a,b with 
                | [x],[y] -> x*y
                | currentU::restU, currentV::restV -> currentU*currentV + multiplyList (restU,restV)
                | _ -> 1.

            let multiply ((M(c)),(M(d))) = c |> List.map 
                                                (fun row -> d |> List.map 
                                                                                (fun col -> multiplyList (row,col)))

            mkMatrix(multiply ((M(a)),(transpose(M(b)))))
    end

    let identityMatrixWithPos x y z = mkMatrix ([[1.0;0.0;0.;x];[0.;1.;0.;y];[0.;0.;1.;z];[0.;0.;0.;1.]]) //CREATES AN IDENTITY MATRIX
    let getList (M(a)) = a
    let getMatrix (T(a,_)) = a
    let getInvMatrix (T(_,b)) = b
    let vectorToMatrix (v:Vector) = mkMatrix ([[v.X];[v.Y];[v.Z];[0.]])
    let pointToMatrix (p:Point) = mkMatrix ([[p.X];[p.Y];[p.Z];[1.]])

    let translate x y z = mkTransformation (identityMatrixWithPos x y z, identityMatrixWithPos -x -y -z)

    let scale width height depth = mkTransformation (
        mkMatrix ([[width;0.0;0.;0.];[0.;height;0.;0.];[0.;0.;depth;0.];[0.;0.;0.;1.]]),
        mkMatrix ([[-width;0.0;0.;0.];[0.;-height;0.;0.];[0.;0.;-depth;0.];[0.;0.;0.;1.]]))
    
    let sheareXY dist = mkMatrix ([[1.;0.;0.;0.];[dist;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    let sheareXZ dist = mkMatrix ([[1.;0.;0.;0.];[0.;1.;0.;0.];[dist;0.;1.;0.];[0.;0.;0.;1.]])
    let sheareYX dist = mkMatrix ([[1.;dist;0.;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    let sheareYZ dist = mkMatrix ([[1.;0.;0.;0.];[0.;1.;0.;0.];[0.;dist;1.;0.];[0.;0.;0.;1.]])
    let sheareZX dist = mkMatrix ([[1.;0.;dist;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    let sheareZY dist = mkMatrix ([[1.;0.;0.;0.];[0.;1.;dist;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])

    //let sheareXYInv dist = mkTransformation ([[1.;0.;0.;0.];[-dist;1.;dist;0.];[0.;0.;1.-dist;0.];[0.;0.;0.;1.]])
    //let sheareXZInv dist = mkTransformation ([[1.;0.;0.;0.];[0.;1.;0.;0.];[-dist;dist;1.;0.];[0.;0.;0.;1.]])
    //let sheareYXInv dist = mkTransformation ([[1.-dist;-dist;0.;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    //let sheareYZInv dist = mkTransformation ([[1.;0.;0.;0.];[-dist;1.;dist;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    //let sheareZXInv dist = mkTransformation ([[1.;0.;0.;0.];[-dist;1.;dist;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    //let sheareZYInv dist = mkTransformation ([[1.;0.;0.;0.];[-dist;1.;dist;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]])
    

    let rotateX angle = mkTransformation(mkMatrix ([[1.;0.;0.;0.];[0.;Math.Cos(angle);-(Math.Sin(angle));0.];[0.;Math.Sin(angle);(Math.Cos(angle));0.];[0.;0.;0.;1.]]),
                                         mkMatrix ([[1.;0.;0.;0.];[0.;Math.Cos(angle);(Math.Sin(angle));0.];[0.;-(Math.Sin(angle));(Math.Cos(angle));0.];[0.;0.;0.;1.]]))
    let rotateY angle = mkTransformation(mkMatrix ([[Math.Cos(angle);0.;Math.Sin(angle);0.];[0.;1.;0.;0.];[-(Math.Sin(angle));0.;Math.Cos(angle);0.];[0.;0.;0.;1.]]),
                                         mkMatrix ([[Math.Cos(angle);0.;-(Math.Sin(angle));0.];[0.;1.;0.;0.];[(Math.Sin(angle));0.;Math.Cos(angle);0.];[0.;0.;0.;1.]]))
    let rotateZ angle = mkTransformation(mkMatrix ([[Math.Cos(angle);-(Math.Sin(angle));0.;0.];[Math.Sin(angle);Math.Cos(angle);0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]),
                                          mkMatrix ([[Math.Cos(angle);(Math.Sin(angle));0.;0.];[-(Math.Sin(angle));Math.Cos(angle);0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]))

    let mergeTransformations (l: Matrix List) = 
        let rec sum (value,l2)= 
            match l2 with
            | first::rest ->
                let v = Matrix.multi(value,first)
                sum(v,rest)
            | _ -> value
        sum (l.Head,l.Tail)


    let matrixToVector (M(a)) = 
        let x = a.Head.Head
        let y = a.Item(1).Head
        let z = a.Item(2).Head
        new Vector(x, y, z)

    let matrixToPoint (M(a)) = 
        let x = a.Head.Head
        let y = a.Item(1).Head
        let z = a.Item(2).Head
        new Point(x, y, z)

    let transformDirectionalLight ((light:DirectionalLight),t) = 
        let matrix = vectorToMatrix (light.GetDirectionFromPoint (new Point(0.,0.,0.)))
        let transMatrix = Matrix.multi (getMatrix(t),matrix)
        matrixToVector transMatrix

    let transformPointLight ((light:PointLight),t) = 
        let matrix = pointToMatrix (light.Position)
        let transMatrix = Matrix.multi (getMatrix(t),matrix)
        matrixToPoint transMatrix

    let transformLight (light:Light) t =
        match light with
        | :? DirectionalLight as d -> DirectionalLight(d.BaseColour, d.Intensity, transformDirectionalLight (d,t)) :> Light
        | :? PointLight as p -> PointLight(p.BaseColour, p.Intensity, transformPointLight (p,t)) :> Light
        | _ -> light

    let transform (s : Sphere) (r : Ray) =  
        failwith("NOT IMPLEMENTED")
    let transform2 (hf : Ray -> HitPoint * Vector) (t: Transformation) = 
       failwith("NOT IMPLEMENTED")

    let transformRay (r : Ray) t = 
        let originMatrix = Matrix.multi (pointToMatrix (r.GetOrigin), getMatrix(t))
        let directionMatrix = Matrix.multi (vectorToMatrix (r.GetDirection), getMatrix(t))
        let origin = matrixToPoint originMatrix
        let direction = matrixToVector directionMatrix
        new Ray(origin, direction)

    let originalHitPoint dist (r:Ray) = 
        r.PointAtTime dist

    let transformNormal (s:Sphere) (p:Point) (t: Transformation)= 
        let vector = s.NormalAtPoint p 
        let tVector = matrixToVector (Matrix.multi (getMatrix(t),(vectorToMatrix vector)))
        tVector
