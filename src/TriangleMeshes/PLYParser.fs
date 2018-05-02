﻿module PLYParser

open FParsec
open System.IO
open System
open Tracer.Basics

type UserState = unit
type Parser<'t> = Parser<'t,UserState>


type Vertex(x,y,z,nx, ny, nz, u, v) = 
    member this.x = x
    member this.y = y
    member this.z = z
    member this.nx = nx
    member this.ny = ny
    member this.nz = nz
    member this.u = u
    member this.v = v


let parse parser str = 
    match run parser str with
    | Success(result,_,_) ->  result
    | Failure (errorMsg,_,_) ->  failwith(errorMsg)

let parseBool parser str = 
    match run parser str with
    | Success(_) ->  true
    | Failure (_) ->  false

let typeParser str = 
    match str with
    | "char" -> 1
    | "uchar" -> 1
    | "int8" -> 1
    | "uint8" -> 1
    | "short" -> 2
    | "int16" -> 2
    | "ushort" -> 2
    | "uint16" -> 2
    | "int" -> 3
    | "int32" -> 3
    | "uint" -> 3
    | "uint32" -> 3
    | "float" -> 4
    | "float32" -> 4
    | "double" -> 5
    | "float64" -> 5
    | _ -> 0

let WhiteSpace = pstring " "

let mutable triangleArray = Array.zeroCreate(1)
let mutable faceArray = Array.zeroCreate(1)

let findVertexFromArray floatsList posArray= 
    let checkArrayIsWithinPLYFile ((a:float list),(b:int array),c) = 
        if (b.[c] = 0) then None else Some a.[b.[c]-1]

    let x = checkArrayIsWithinPLYFile(floatsList,posArray,0)
    let y = checkArrayIsWithinPLYFile(floatsList,posArray,1)
    let z = checkArrayIsWithinPLYFile(floatsList,posArray,2)
    let nx = checkArrayIsWithinPLYFile(floatsList,posArray,3)
    let ny = checkArrayIsWithinPLYFile(floatsList,posArray,4)
    let nz = checkArrayIsWithinPLYFile(floatsList,posArray,5)
    let u = checkArrayIsWithinPLYFile(floatsList,posArray,6)
    let v = checkArrayIsWithinPLYFile(floatsList,posArray,7)
    new Vertex(x,y,z,nx,ny,nz,u,v)

let parsePLY (filepath:string) = 
    let sr = new StreamReader(filepath)
    let mutable parsing = true
    if (not sr.EndOfStream) then
        let mutable nextLine = sr.ReadLine()
        let mutable numberOfProperty = 1
        let mutable propertyList = []
        let result = parseBool (pstring "ply") (nextLine)
        match result with

        | true -> 
            //printfn "Started Parsing..."

            nextLine <- sr.ReadLine()

            let isAscii = parseBool (pstring "format ascii " .>> pfloat) (nextLine)
            let formatBoolean = (pstring "format binary_little_endian " .>> pfloat) 
            let isBigEndian = parseBool (pstring "format binary_big_endian " .>> pfloat) nextLine
            let isBoolean = (parseBool (formatBoolean) (nextLine)) || isBigEndian
            

            if (not (isBoolean) && not isAscii) then failwith ("Parsing Error: TAMPERED PLY FILE")

            nextLine <- sr.ReadLine()
            let arraySizeParser = pstring "element vertex " >>. pint32
            let isArraySize s = parseBool arraySizeParser s
            while (not (isArraySize nextLine)) do
                nextLine <- sr.ReadLine()

            let arraySize = parse arraySizeParser nextLine
            triangleArray <- Array.zeroCreate arraySize
            let isEndOfHeader s = parseBool (pstring "end_header") s
            let vertexPosition : int array = Array.zeroCreate 9
            let readPropertyParser = 
                nextLine <- sr.ReadLine()
                let startWithProperty (s:string) = parseBool (pstring "property" .>> (anyString (s.Length-8))) s
                while (startWithProperty nextLine) do
                    let test = nextLine.Split [|' '|]
                    match test.[2] with
                        | "x" -> vertexPosition.[0] <- numberOfProperty
                        | "y" -> vertexPosition.[1] <- numberOfProperty
                        | "z" -> vertexPosition.[2] <- numberOfProperty
                        | "nx" -> vertexPosition.[3] <- numberOfProperty
                        | "ny" -> vertexPosition.[4] <- numberOfProperty
                        | "nz" -> vertexPosition.[5] <- numberOfProperty
                        | "u" -> vertexPosition.[6] <- numberOfProperty
                        | "v" -> vertexPosition.[7] <- numberOfProperty
                        | _ -> vertexPosition.[8] <- numberOfProperty
                    let typeVal = typeParser test.[1]
                    propertyList <- List.append propertyList [typeVal]
                    nextLine <- sr.ReadLine()
                    numberOfProperty <- numberOfProperty + 1
            readPropertyParser

            let faceLength = parse (pstring "element face " >>. pint32) nextLine
            faceArray <- Array.zeroCreate faceLength
            nextLine <- sr.ReadLine()
            while (not (isEndOfHeader nextLine)) do
                let parseListProps = pstring "property list " >>. (anyString (nextLine.Length-28)) .>> pstring "vertex_indices"
                nextLine <- sr.ReadLine()
            nextLine <- sr.ReadLine()

            match isAscii,isBoolean with
            | true,_ -> 
                for i in 0..triangleArray.Length-1 do
                    nextLine <- nextLine.Substring(0,nextLine.Length-1)
                    let listFloatParser = (sepBy pfloat WhiteSpace)
                    let listFloat = parse listFloatParser nextLine

                    triangleArray.[i] <- findVertexFromArray listFloat vertexPosition
                    nextLine <- sr.ReadLine()
                for i in 0..faceArray.Length-1 do 
                    nextLine <- nextLine.Substring(0,nextLine.Length-1)
                    let listIntParser = (sepBy pint32 WhiteSpace)
                    let listInt = parse listIntParser nextLine
                    faceArray.[i] <- listInt
                    nextLine <- sr.ReadLine()
            | _,true -> 
                printfn ("BINARY START")
                let br = new BinaryReader(sr.BaseStream)
                //Parsing Vertices
                for j in 0..(triangleArray.Length-1) do 
                    let vertexProps = Array.zeroCreate(numberOfProperty)
                    for k in 0..(propertyList.Length-1) do
                        let f = 
                            match propertyList.[k] with
                            | 1 -> 
                                let buffer : byte[] = Array.zeroCreate(1)
                                br.Read(buffer,0,1)
                                if(isBigEndian) then
                                    Array.Reverse(buffer)
                                (float buffer.[0])
                            | 2 -> 
                                let buffer : byte[] = Array.zeroCreate(2)
                                br.Read(buffer,0,2)
                                if(isBigEndian) then
                                    Array.Reverse(buffer)
                                (float (BitConverter.ToInt16 (buffer, 0)))
                            | 3 -> 
                                let buffer : byte[] = Array.zeroCreate(4)
                                br.Read(buffer,0,4)
                                if(isBigEndian) then
                                    Array.Reverse(buffer)
                                (float (BitConverter.ToInt32 (buffer, 0)))
                            | 4 -> 
                                let buffer : byte[] = Array.zeroCreate(4)
                                br.Read(buffer,0,4)
                                if(isBigEndian) then
                                    Array.Reverse(buffer)
                                (float (BitConverter.ToSingle (buffer, 0)))
                            | 5 -> 
                                let buffer : byte[] = Array.zeroCreate(8)
                                br.Read(buffer,0,8)
                                if(isBigEndian) then
                                    Array.Reverse(buffer)
                                (float (BitConverter.ToDouble (buffer, 0)))
                            | _ -> 0.0
                        vertexProps.[k] <- f
                    triangleArray.[j] <- findVertexFromArray (vertexProps |> Array.toList) vertexPosition
                //Parsing Triangles
                let dump : byte[] = Array.zeroCreate(11)
                br.Read(dump,0,11)
                for j in 0..faceArray.Length-1 do 
                    let lengthBuffer : byte[] = Array.zeroCreate(1)
                    br.Read(lengthBuffer, 0,1)
                    let numbers = Array.zeroCreate(4)

                    numbers.[0] <- (int lengthBuffer.[0])

                    for k in 0..2 do 
                        let faceIdBuffer : byte[] = Array.zeroCreate(4)
                        br.Read(faceIdBuffer, 0, 4)

                        if(isBigEndian) then Array.Reverse(faceIdBuffer)

                        let faceId = BitConverter.ToInt32(faceIdBuffer,0)
                        numbers.[k+1] <- (int faceId)
                    printfn "%A" numbers
                    faceArray.[j] <- (Array.toList numbers)
            | _,_ -> failwith ("Parsing Error: TAMPERED PLY FILE")
            printfn "...Parsing Done"
        | false -> parsing <- false

let drawTriangles (filepath:string)= 
    let test = parsePLY filepath
    let material = MatteMaterial(Colour.Red)
    let ar = Array.zeroCreate(faceArray.Length)
    for i in 0..faceArray.Length-1 do 
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new Point(v1.x.Value,v1.y.Value,v1.z.Value)
        
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new Point(v2.x.Value,v2.y.Value,v2.z.Value)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new Point(v3.x.Value,v3.y.Value,v3.z.Value)
        ar.[i] <- ((new Triangle(p1,p2,p3, material) :> Shape))
    ar

let drawNumberOfTriangles (filepath:string) n= 
    let test = parsePLY filepath
    let material = MatteMaterial(Colour.Red)
    let ar = Array.zeroCreate(n)
    for i in 0..n-1 do 
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new Point(v1.x.Value,v1.y.Value,v1.z.Value)
        
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new Point(v2.x.Value,v2.y.Value,v2.z.Value)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new Point(v3.x.Value,v3.y.Value,v3.z.Value)
        ar.[i] <- ((new Triangle(p1,p2,p3, material) :> Shape))
    ar