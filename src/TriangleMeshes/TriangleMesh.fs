module TriangleMesh

open Tracer.Basics
open Acceleration.KD_tree
open System.Threading.Tasks
open PLYParser

let createTriangles (triangleArray : Vertex array) (faceArray : int list array) material = 
    let ar = Array.zeroCreate(faceArray.Length-1)
    for i in 0..(ar.Length-1) do 
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new Point(v1.x.Value,v1.y.Value,v1.z.Value)
        
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new Point(v2.x.Value,v2.y.Value,v2.z.Value)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new Point(v3.x.Value,v3.y.Value,v3.z.Value)
        ar.[i] <- ((new Triangle(p1,p2,p3, material) :> Shape))
    ar

let smoothTriangles (tA : Triangle array) = 
    match tA.Length with
    | 1 -> (tA.[0]).n
    | _ ->
        let mutable sumOfNormals = (tA.[0]).n
        for i in 1..(tA.Length-1) do 
            sumOfNormals <-  sumOfNormals |> ( + ) tA.[i].n
        sumOfNormals
        

let drawTriangles (filepath:string) (smoothen:bool) (withKDTree : bool)= 
    let test = parsePLY filepath
    let triangleArray = fst test
    let faceArray = snd test

    let material = MatteMaterial(Colour.Red)

    let ar = createTriangles triangleArray faceArray material
    
    //if (smoothen) then 
    //    let newAr = smoothTriangles ar

    if (withKDTree) then 
        let kdTree = buildKDTree (ar)
        let sh = {new Shape() with
            member this.hitFunction r = 
                traverseKDTree kdTree r ar
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
                    if (s.Time < smallestTime) then bestHit <- s
                bestHit
            member this.getBoundingBox () = failwith "I hate this"
            member this.isInside p = failwith "Need some time to think this through"
        }
        sh

let drawTrianglesSpecificNumber numberOfShapes (filepath:string) (smoothen:bool) (withKDTree : bool)= 
    let test = parsePLY filepath
    let triangleArray = fst test
    let faceArray = snd test

    let material = MatteMaterial(Colour.Red)

    let ar = Array.zeroCreate(numberOfShapes)
    for i in 0..(numberOfShapes-1) do 
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new Point(v1.x.Value,v1.y.Value,v1.z.Value)
        
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new Point(v2.x.Value,v2.y.Value,v2.z.Value)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new Point(v3.x.Value,v3.y.Value,v3.z.Value)
        ar.[i] <- ((new Triangle(p1,p2,p3, material) :> Shape))
  
    //if (smoothen) then 
    //    let newAr = smoothTriangles ar

    if (withKDTree) then 
        let kdTree = buildKDTree (ar)
        let sh = {new Shape() with
            member this.hitFunction r = 
                traverseKDTree kdTree r ar
            member this.getBoundingBox () = failwith "I hate this"
            member this.isInside p = failwith "Maybe kdTree has some function for this"
        }
        sh
    else 
        let sh = {new Shape() with
            member this.hitFunction r = 
                let mutable bestHit = HitPoint(r)
                let mutable smallestTime = 2147483647.
                for i in 0..(numberOfShapes-1) do 
                    let s = (ar.[i]).hitFunction r
                    if (s.Time < smallestTime && s.DidHit) then 
                        bestHit <- s
                        smallestTime <- s.Time
                bestHit
            member this.getBoundingBox () = failwith "I hate this"
            member this.isInside p = failwith "Need some time to think this through"
        }
        sh