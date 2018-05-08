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
        

let drawTriangles (filepath:string) (smoothen:bool) (mat : Material) = 
    let test = parsePLY filepath
    let triangleArray = fst test
    let faceArray = snd test

    let material = mat
    let ar = createTriangles triangleArray faceArray material smoothen

    let accel = Acceleration.createAcceleration(ar)
    let sh = {new Shape() with
        member this.hitFunction r = 
            let hit = traverseIAcceleration accel r ar
            if(smoothen) then
                match hit.Shape with
                | :? Triangle -> 
                    let triangle = hit.Shape :?> Triangle
                    let alpha =  1. - triangle.beta - triangle.gamma
                    let na = (triangle.a :?> TriPoint).v.normal.Normalise
                    let nb = (triangle.b :?> TriPoint).v.normal.Normalise
                    let nc = (triangle.c :?> TriPoint).v.normal.Normalise
                    let V = (( * ) alpha na) |> ( + ) (( * ) triangle.beta nb) |> ( + ) (( * ) triangle.gamma nc)
                    let normal = V.Normalise
                    HitPoint(r, hit.Time, normal, hit.Material, hit.Shape)
                | _ -> hit
            else hit
        member this.getBoundingBox () = Acceleration.getAccelBoundingBox accel
        member this.isInside p = failwith "Maybe kdTree has some function for this"
    }
    sh