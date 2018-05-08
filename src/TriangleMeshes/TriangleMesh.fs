module TriangleMesh

open Tracer.Basics
open Acceleration.KD_tree
open System.Threading.Tasks
open PLYParser

let createTriangles (triangleArray : Vertex array) (faceArray : int list array) material (map : Map<float*float*float, Vertex>) = 
    let ar = Array.zeroCreate(faceArray.Length-1)
    for i in 0..(ar.Length-1) do 
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new Point(v1.x.Value,v1.y.Value,v1.z.Value)
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new Point(v2.x.Value,v2.y.Value,v2.z.Value)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new Point(v3.x.Value,v3.y.Value,v3.z.Value)
        let triangle = Triangle(p1,p2,p3, material)
        map.Add((p1.X, p1.Y, p1.Z), (v1)) |> ignore
        map.Add((p2.X, p2.Y, p2.Z), (v2)) |> ignore
        map.Add((p3.X, p3.Y, p3.Z), (v3)) |> ignore
        v1.normal <- triangle.n
        v2.normal <- triangle.n
        v3.normal <- triangle.n
        ar.[i] <- (triangle :> Shape)
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
    let map = Map.empty
    let ar = createTriangles triangleArray faceArray material map
    
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

                    if (s.Time < smallestTime && s.DidHit) then 
                        if (smoothen) then
                            let triangle = ar.[i] :?> Triangle
                            let alpha =  1. - triangle.beta - triangle.gamma
                            let normalOfTri = triangle.n.Normalise
                            let v1 : Vertex = map.Item(triangle.a.X, triangle.a.Y, triangle.a.Z)
                            let v2 : Vertex = map.Item(triangle.b.X, triangle.b.Y, triangle.b.Z)
                            let v3 : Vertex = map.Item(triangle.c.X, triangle.c.Y, triangle.c.Z)
                            let V = v1.normal.MultScalar(alpha) |> ( + ) (v2.normal.MultScalar(triangle.beta) |> ( + ) (v3.normal.MultScalar(triangle.gamma)))
                            let normal = V.Normalise
                            bestHit <- HitPoint(r, s.Time, normal, material)
                        else bestHit <- s
                bestHit
            member this.getBoundingBox () = failwith "I hate this"
            member this.isInside p = failwith "Need some time to think this through"
        }
        sh

let drawTrianglesSpecificNumber numberOfShapes (filepath:string) (smoothen:bool) (withKDTree : bool)= 
    let test = parsePLY filepath
    printfn "parsing done"
    let triangleArray = fst test
    let faceArray = snd test

    let material = MatteMaterial(Colour.Red)
    let map = Map.empty
    printfn "kage"
    let ar = Array.zeroCreate(numberOfShapes)
    for i in 0..(numberOfShapes-1) do 
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new Point(v1.x.Value,v1.y.Value,v1.z.Value)
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new Point(v2.x.Value,v2.y.Value,v2.z.Value)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new Point(v3.x.Value,v3.y.Value,v3.z.Value)
        let triangle = Triangle(p1,p2,p3, material)
        printfn "test"
        if (smoothen) then
            map.Add((p1.X, p1.Y, p1.Z), (v1)) |> ignore
            map.Add((p2.X, p2.Y, p2.Z), (v2)) |> ignore
            map.Add((p3.X, p3.Y, p3.Z), (v3)) |> ignore
        let norm = triangle.n
        v1.normal <- norm
        printfn "test"
        v2.normal <- triangle.n
        v3.normal <- triangle.n
        ar.[i] <- ((new Triangle(p1,p2,p3, material) :> Shape))
        printfn "test"
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

                    if (s.Time < smallestTime && s.DidHit) then 
                        if (smoothen) then
                            let triangle = ar.[i] :?> Triangle
                            let alpha =  1. - triangle.beta - triangle.gamma
                            let normalOfTri = triangle.n.Normalise
                            let v1 : Vertex = map.Item(triangle.a.X, triangle.a.Y, triangle.a.Z)
                            let v2 : Vertex = map.Item(triangle.b.X, triangle.b.Y, triangle.b.Z)
                            let v3 : Vertex = map.Item(triangle.c.X, triangle.c.Y, triangle.c.Z)
                            let V = v1.normal.MultScalar(alpha) |> ( + ) (v2.normal.MultScalar(triangle.beta) |> ( + ) (v3.normal.MultScalar(triangle.gamma)))
                            let normal = V.Normalise
                            bestHit <- HitPoint(r, s.Time, normal, material)
                        else bestHit <- s
                bestHit
            member this.getBoundingBox () = failwith "I hate this"
            member this.isInside p = failwith "Need some time to think this through"
        }
        sh