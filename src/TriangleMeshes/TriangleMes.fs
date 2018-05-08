module TriangleMes

open Tracer.Basics
open Tracer.Basics.Acceleration
open System.Threading.Tasks
open PLYParser
open Tracer.BaseShape

type TriPoint (v : Vertex) = 
    inherit Point(v.x.Value,v.y.Value,v.z.Value)
    member this.v : Vertex = v


let createTriangles (triangleArray : Vertex array) (faceArray : int list array) material (smooth:bool) = 
    let ar = Array.zeroCreate(faceArray.Length)
    for i in 0..(ar.Length-1) do 
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new TriPoint(v1)
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new TriPoint(v2)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new TriPoint(v3)
        let triangle = Triangle((p1 :> Point) ,(p2 :> Point) ,(p3 :> Point), material)
        if (smooth) then
            v1.normal <- triangle.n
            v2.normal <- triangle.n
            v3.normal <- triangle.n
        ar.[i] <- (triangle :> Shape)
    ar
        

let drawTriangles (filepath:string) (smoothen:bool) (withAcceleration : bool) (mat : Material) = 
    let test = parsePLY filepath
    let triangleArray = fst test
    let faceArray = snd test

    let material = mat
    let ar = createTriangles triangleArray faceArray material smoothen

    if (withAcceleration) then 
        let accel = Acceleration.createAcceleration(ar)
        let sh = {new Shape() with
            member this.hitFunction r = 
                traverseIAcceleration accel r ar
            member this.getBoundingBox () = BBox (Point(-1.,-1.,-1.),Point(1.,1.,1.))
            member this.isInside p = failwith "Maybe kdTree has some function for this"
        }
        sh
    else 
        let sh = {new Shape() with
            member this.hitFunction r = 
                let mutable bestHit = HitPoint(r)
                let mutable smallestTime = 2147483647.
                for i in 0..(ar.Length-1) do 
                    let s = (ar.[i]).hitFunction r

                    if (s.Time < smallestTime && s.DidHit) then 
                        if (smoothen) then
                            let triangle = ar.[i] :?> Triangle
                            let alpha =  1. - triangle.beta - triangle.gamma
                            let na = (triangle.a :?> TriPoint).v.normal.Normalise
                            let nb = (triangle.b :?> TriPoint).v.normal.Normalise
                            let nc = (triangle.c :?> TriPoint).v.normal.Normalise
                            let V = (( * ) alpha na) |> ( + ) (( * ) triangle.beta nb) |> ( + ) (( * ) triangle.gamma nc)
                            let normal = V.Normalise
                            bestHit <- HitPoint(r, s.Time, normal, material, s.Shape)
                        else bestHit <- s
                bestHit
            member this.getBoundingBox () = BBox (Point(-1.,-1.,-1.),Point(1.,1.,1.))
            member this.isInside p = failwith "Need some time to think this through"
        }
        sh

let drawTrianglesSpecificNumber numberOfShapes (filepath:string) (smoothen:bool) (withAcceleration : bool)= 
    let test = parsePLY filepath
    let triangleArray = fst test
    let faceArray = snd test

    let material = MatteMaterial(Colour.Red)
    let ar = Array.zeroCreate(numberOfShapes)
    for i in 0..(numberOfShapes-1) do 
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new TriPoint(v1)
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new TriPoint(v2)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new TriPoint(v3)
        let triangle = Triangle(p1,p2,p3, material)
        v1.normal <- triangle.n
        v2.normal <- triangle.n
        v3.normal <- triangle.n
        ar.[i] <- (triangle :> Shape)

    if (withAcceleration) then 
        let accel = createAcceleration(ar)
        let sh = {new Shape() with
            member this.hitFunction r = 
                traverseIAcceleration accel r ar
            member this.getBoundingBox () = failwith "I hate this"
            member this.isInside p = failwith "Maybe kdTree has some function for this"
        }
        sh
    else 
        let sh = {new Shape() with
            member this.hitFunction r =
                let mutable bestHit = HitPoint(r)
                let mutable smallestTime = 2147483647.
                for i in 0..(ar.Length-1) do 
                    let s = (ar.[i]).hitFunction r
                    if (s.Time < smallestTime && s.DidHit) then 
                        if (smoothen) then
                            let triangle = ar.[i] :?> Triangle
                            let alpha =  1. - triangle.beta - triangle.gamma
                            let na = (triangle.a :?> TriPoint).v.normal.Normalise
                            let nb = (triangle.b :?> TriPoint).v.normal.Normalise
                            let nc = (triangle.c :?> TriPoint).v.normal.Normalise
                            let V = (( * ) alpha na) |> ( + ) (( * ) triangle.beta nb) |> ( + ) (( * ) triangle.gamma nc)
                            let normal = V.Normalise
                            if (i = 0) then 
                                printfn "old normal %A current normal %A" s.Normal normal
                            bestHit <- HitPoint(r, s.Time, normal, material, s.Shape)
                        else
                            bestHit <- s
                bestHit
            member this.getBoundingBox () = failwith "I hate this"
            member this.isInside p = failwith "Need some time to think this through"
        }
        sh