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

    let scale width height depth = mkTransformation (mkMatrix ([[width;0.0;0.;0.];[0.;height;0.;0.];[0.;0.;depth;0.];[0.;0.;0.;1.]]),
                                                     mkMatrix ([[Math.Pow(width,-1.);0.0;0.;0.];[0.;Math.Pow(height,-1.);0.;0.];[0.;0.;Math.Pow(depth,-1.);0.];[0.;0.;0.;1.]]))
    let sheare (xy,xz,yx,yz,zx,zy) = 
        let matrix = mkMatrix([[1.;yx;zx;0.];[xy;1.;zy;0.];[xz;yz;1.;0.];[0.;0.;0.;1.]])
        let det = (1.-(xy*yx)+(xz*zx)-(yz*zy)+(xy*yz*zx)+(xz*yz*zy))
        let mult = 1./det
        //TODO: Ask what is wrong with the inverse
        let inv = 
            mkMatrix (
                [[mult*(1.-(yz*zy));mult*(-yx+yz*zx);mult*(-zx+yx*zy);0.];
                [mult*(-xy+xz*zy);mult*(1.-xz*zx);mult*(-zy+xy*zx);0.];
                [mult*(-xz+xy*yz);mult*(-yz+xz*yx);mult*(1.-xy*yx);0.];
                [0.;0.;0.;mult*det]])
        mkTransformation(matrix,inv)

    let rotateX angle = mkTransformation(mkMatrix ([[1.;0.;0.;0.];[0.;Math.Cos(angle);-(Math.Sin(angle));0.];[0.;Math.Sin(angle);(Math.Cos(angle));0.];[0.;0.;0.;1.]]),
                                         mkMatrix ([[1.;0.;0.;0.];[0.;Math.Cos(angle);(Math.Sin(angle));0.];[0.;-(Math.Sin(angle));(Math.Cos(angle));0.];[0.;0.;0.;1.]]))
    let rotateY angle = mkTransformation(mkMatrix ([[Math.Cos(angle);0.;Math.Sin(angle);0.];[0.;1.;0.;0.];[-(Math.Sin(angle));0.;Math.Cos(angle);0.];[0.;0.;0.;1.]]),
                                         mkMatrix ([[Math.Cos(angle);0.;-(Math.Sin(angle));0.];[0.;1.;0.;0.];[(Math.Sin(angle));0.;Math.Cos(angle);0.];[0.;0.;0.;1.]]))
    let rotateZ angle = mkTransformation(mkMatrix ([[Math.Cos(angle);-(Math.Sin(angle));0.;0.];[Math.Sin(angle);Math.Cos(angle);0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]),
                                          mkMatrix ([[Math.Cos(angle);(Math.Sin(angle));0.;0.];[-(Math.Sin(angle));Math.Cos(angle);0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]]))
                                          


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


    let transformRay (r : Ray) t = 
        let originMatrix = Matrix.multi (pointToMatrix (r.GetOrigin), getInvMatrix(t))
        let directionMatrix = Matrix.multi (vectorToMatrix (r.GetDirection), getInvMatrix(t))
        let origin = matrixToPoint originMatrix
        let direction = matrixToVector directionMatrix
        new Ray(origin, direction)

    let originalHitPoint dist (r:Ray) = 
        r.PointAtTime dist

    let transformNormal (v:Vector) (t: Transformation)= 
        let vector = v
        let tVector = matrixToVector (Matrix.multi (transpose (getInvMatrix(t)),(vectorToMatrix vector)))
        tVector

    let transform (s : Shape) (t:Transformation) =  
        //let tranShape = new Shape()
        
        //override tranShape.hitFunction (r:Ray) = 
        //    let transformedRay = transformRay r t
        //    let hitsOriginal = s.hitFunction transformedRay
        //    let hitPoint = r.PointAtTime hitsOriginal.dist
        //    let normal = transformNormal  hitPoint.normal t
        //    new Hitpoint (r, hitsOriginal.time,normal,s.material)
        failwith("NOT IMPLEMENTED")
    let transform2 (hf : Ray -> HitPoint * Vector) (t: Transformation) = 
       failwith("NOT IMPLEMENTED")
