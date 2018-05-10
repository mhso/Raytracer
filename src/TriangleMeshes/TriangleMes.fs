module TriangleMes

open Tracer.Basics
open Tracer.Basics.Acceleration
open System.Threading.Tasks
open PLYParser
open Tracer.BaseShape

type TriPoint (v : Vertex) = 
    inherit Point(v.x,v.y,v.z)
    member this.v : Vertex = v


let createTriangles (triangleArray : Vertex array) (faceArray : int list array) (smooth:bool) (hasNormalWithin : bool)= 
    let ar = Array.zeroCreate(faceArray.Length)
    for i in 0..(ar.Length-1) do 
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new TriPoint(v1)
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new TriPoint(v2)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new TriPoint(v3)
        let triangle = new BaseTriangle((p1 :> Point) ,(p2 :> Point) ,(p3 :> Point))
        if (smooth && not hasNormalWithin) then
            v1.normal <- triangle.n
            v2.normal <- triangle.n
            v3.normal <- triangle.n
        ar.[i] <- (triangle)
    ar
        

let drawTriangles (filepath:string) (smoothen:bool) = 
    let test = parsePLY filepath
    let triangleArray = fst test
    let faceArray = snd test
    let hasNormalWithin = triangleArray.[0].nx.IsSome
    let hasTexture = triangleArray.[0].u.IsSome

    let ar = createTriangles triangleArray faceArray smoothen hasNormalWithin

    let baseShape = {new BaseShape() with
        member this.toShape(tex) = 
            let newTriangle = Array.zeroCreate(ar.Length)
            let firstTriangle = (ar.[0]).toShape(tex)
            newTriangle.[0] <- firstTriangle
            let mutable lowPoint = firstTriangle.getBoundingBox().lowPoint
            let mutable highPoint = firstTriangle.getBoundingBox().highPoint

            for i in 1..(ar.Length-1) do
                let triangle = (ar.[i]).toShape(tex)
                let triangleLowPoint = triangle.getBoundingBox().lowPoint 
                let triangleHightPoint = triangle.getBoundingBox().highPoint
                if(triangleLowPoint.X < lowPoint.X) then lowPoint <- Point(triangleLowPoint.X, lowPoint.Y, lowPoint.Z)
                if(triangleLowPoint.Y < lowPoint.Y) then lowPoint <- Point(lowPoint.X, triangleLowPoint.Y, lowPoint.Z)
                if(triangleLowPoint.Z < lowPoint.Z) then lowPoint <- Point(lowPoint.X, lowPoint.Y, triangleLowPoint.Z)
                if(triangleHightPoint.X > highPoint.X) then highPoint <- Point(triangleHightPoint.X, highPoint.Y, highPoint.Z)
                if(triangleHightPoint.Y > highPoint.Y) then highPoint <- Point(highPoint.X, triangleHightPoint.Y, highPoint.Z)
                if(triangleHightPoint.Z > highPoint.Z) then highPoint <- Point(highPoint.X, highPoint.Y, triangleHightPoint.Z)
                newTriangle.[i] <- triangle
            let accel = Acceleration.createAcceleration(newTriangle)
            let shape = {new Shape() with
                member this.hitFunction r = 
                    let hit = traverseIAcceleration accel r newTriangle
                    let finalHit = 
                        if(smoothen) then
                            match hit.Shape with
                            | :? Triangle -> 
                                let triangle = hit.Shape :?> Triangle
                                let alpha =  1. - triangle.beta - triangle.gamma
                                let vertexNormal = 
                                    if (hasNormalWithin) then 
                                        let na = 
                                            let triangle = (triangle.a :?> TriPoint).v
                                            Vector(triangle.nx.Value,triangle.ny.Value,triangle.nz.Value)
                                        let nb = 
                                            let triangle = (triangle.b :?> TriPoint).v
                                            Vector(triangle.nx.Value,triangle.ny.Value,triangle.nz.Value)
                                        let nc =
                                            let triangle = (triangle.b :?> TriPoint).v
                                            Vector(triangle.nx.Value,triangle.ny.Value,triangle.nz.Value)
                                        [|na;nb;nc|]
                                    else
                                        let na = (triangle.a :?> TriPoint).v.normal.Normalise
                                        let nb = (triangle.b :?> TriPoint).v.normal.Normalise
                                        let nc = (triangle.c :?> TriPoint).v.normal.Normalise
                                        [|na;nb;nc|]
                                let V = (( * ) alpha vertexNormal.[0] |> ( + ) (( * ) triangle.beta vertexNormal.[1]) |> ( + ) (( * ) triangle.gamma vertexNormal.[2]))
                                let normal = V.Normalise
                                HitPoint(r, hit.Time, normal, hit.Material, hit.Shape)
                            | _ -> hit
                        else hit
                    match finalHit.Shape with
                    | :? Triangle -> 
                        let triangle = hit.Shape :?> Triangle
                        if (hasTexture) then
                            let triA = (triangle.a :?> TriPoint).v
                            let triB = (triangle.b :?> TriPoint).v
                            let triC = (triangle.c :?> TriPoint).v
                            let alpha = 1. - triangle.beta - triangle.gamma
                            let test = alpha + triangle.beta + triangle.gamma
                            let v = (alpha * triA.v.Value) + (triangle.beta * triB.v.Value) + (triangle.gamma * triC.v.Value)
                            let u = (alpha * triA.u.Value) + (triangle.beta * triB.u.Value) + (triangle.gamma * triC.u.Value)
                            if(u < 0.0 || u > 1.0 || v < 0.0 || v > 1.0) then printfn "alpha = %A beta = %A gamme = %A values : %A :::::: %A" test triangle.beta triangle.gamma v u
                            let textureMati = ((Textures.getFunc tex) v u)
                            HitPoint(r, finalHit.Time, finalHit.Normal, textureMati, finalHit.Shape)
                        else finalHit
                    | _ -> finalHit
                member this.getBoundingBox () = BBox(lowPoint, highPoint)
                member this.isInside p = failwith "Maybe kdTree has some function for this"
            }
            shape
    }
    baseShape