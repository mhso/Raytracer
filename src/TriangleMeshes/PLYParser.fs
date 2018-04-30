﻿module PLYParser

open FParsec
open System.IO
open System

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
    | Success(_,_,_) ->  true
    | Failure (_,_,_) ->  false

let WhiteSpace = pstring " "

let parseHeader (sr:StreamReader) = 
    let mutable nextLine = sr.ReadLine()
    let isComment (s:string) = parseBool (pstring "comment" .>> (anyString (s.Length-7))) s
    while (isComment nextLine) do 
        nextLine <- sr.ReadLine()
    let arraySizeParser = pstring "element vertex " >>. pint32
    let arraySize = parse arraySizeParser nextLine
    let triangleArray = Array.zeroCreate arraySize
    let isEndOfHeader s = parseBool (pstring "end_header") s
    let vertexPosition : int array = Array.zeroCreate 9
    let readPropertyParser = 
        let mutable i = 1
        nextLine <- sr.ReadLine()
        let startWithProperty (s:string) = parseBool (pstring "property" .>> (anyString (s.Length-8))) s
        while (startWithProperty nextLine) do
            let endChars = nextLine.Substring(nextLine.Length-2)
            match endChars with
                | " x" -> vertexPosition.[0] <- i
                | " y" -> vertexPosition.[1] <- i
                | " z" -> vertexPosition.[2] <- i
                | "nx" -> vertexPosition.[3] <- i
                | "ny" -> vertexPosition.[4] <- i
                | "nz" -> vertexPosition.[5] <- i
                | " u" -> vertexPosition.[6] <- i
                | " v" -> vertexPosition.[7] <- i
                | _ -> vertexPosition.[8] <- i
            nextLine <- sr.ReadLine()
            i <- i + 1
    readPropertyParser
    let faceLength = parse (pstring "element face " >>. pint32) nextLine
    let faceArray = Array.zeroCreate faceLength
    nextLine <- sr.ReadLine()
    while (not (isEndOfHeader nextLine)) do
        nextLine <- sr.ReadLine()
    nextLine <- sr.ReadLine()
    (triangleArray, faceArray, vertexPosition)

let first (a:Vertex[],b:int list[],c:int array) = a
let second (a:Vertex[],b:int list[],c:int array) = b
let third (a:Vertex[],b:int list[],c:int array) = c

let parsePLY (filepath:string) = 
    let sr = new StreamReader(filepath)
    let mutable parsing = true
    if (not sr.EndOfStream) then
        let mutable nextLine = sr.ReadLine()
        let result = parseBool (pstring "ply") (nextLine)
        match result with
        | true -> 
            printfn "Started Parsing..."
            nextLine <- sr.ReadLine()
            let isAscii = parseBool (pstring "format ascii 1.0") (nextLine)
            let formatBoolean = many ((pstring "format binary_little_endian 1.0") <|> (pstring "format binary_big_endian 1.0"))
            let isBoolean = parseBool (formatBoolean) (nextLine)
            match isAscii,isBoolean with
            | true,_ -> 
                //let header = (parseHeader (sr))
                //let triangleArray = first(header)
                //let faceArray = second(header)
                //let vertexPosition = third(header)
                nextLine <- sr.ReadLine()
                let isComment (s:string) = parseBool (pstring "comment" .>> (anyString (s.Length-7))) s
                while (isComment nextLine) do 
                    nextLine <- sr.ReadLine()
                let arraySizeParser = pstring "element vertex " >>. pint32
                let arraySize = parse arraySizeParser nextLine
                let triangleArray = Array.zeroCreate arraySize
                let isEndOfHeader s = parseBool (pstring "end_header") s
                let vertexPosition : int array = Array.zeroCreate 9
                let readPropertyParser = 
                    let mutable i = 1
                    nextLine <- sr.ReadLine()
                    let startWithProperty (s:string) = parseBool (pstring "property" .>> (anyString (s.Length-8))) s
                    while (startWithProperty nextLine) do
                        let endChars = nextLine.Substring(nextLine.Length-2)
                        match endChars with
                            | " x" -> vertexPosition.[0] <- i
                            | " y" -> vertexPosition.[1] <- i
                            | " z" -> vertexPosition.[2] <- i
                            | "nx" -> vertexPosition.[3] <- i
                            | "ny" -> vertexPosition.[4] <- i
                            | "nz" -> vertexPosition.[5] <- i
                            | " u" -> vertexPosition.[6] <- i
                            | " v" -> vertexPosition.[7] <- i
                            | _ -> vertexPosition.[8] <- i
                        nextLine <- sr.ReadLine()
                        i <- i + 1
                readPropertyParser
                let faceLength = parse (pstring "element face " >>. pint32) nextLine
                let faceArray = Array.zeroCreate faceLength
                nextLine <- sr.ReadLine()
                while (not (isEndOfHeader nextLine)) do
                    nextLine <- sr.ReadLine()
                nextLine <- sr.ReadLine()
                for i in 0..triangleArray.Length-1 do
                    nextLine <- nextLine.Substring(0,nextLine.Length-1)
                    let listFloatParser = (sepBy pfloat WhiteSpace)
                    let listFloat = parse listFloatParser nextLine
                    let checkArrayIsWithinPLYFile ((a:float list),(b:int array),c) = if (b.[c] = 0) then None else Some a.[b.[c]-1]

                    let x = checkArrayIsWithinPLYFile(listFloat,vertexPosition,0)
                    let y = checkArrayIsWithinPLYFile(listFloat,vertexPosition,1)
                    let z = checkArrayIsWithinPLYFile(listFloat,vertexPosition,2)
                    let nx = checkArrayIsWithinPLYFile(listFloat,vertexPosition,3)
                    let ny = checkArrayIsWithinPLYFile(listFloat,vertexPosition,4)
                    let nz = checkArrayIsWithinPLYFile(listFloat,vertexPosition,5)
                    let u = checkArrayIsWithinPLYFile(listFloat,vertexPosition,6)
                    let v = checkArrayIsWithinPLYFile(listFloat,vertexPosition,7)

                    triangleArray.[i] <- new Vertex(x,y,z,nx,ny,nz,u,v)
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
                let buffer : byte[] = Array.zeroCreate(4)
                br.Read(buffer,0,4)
                let float = BitConverter.ToSingle(buffer, 0)
                while (not (sr.EndOfStream)) do 
                    let str = br.ReadString
                    printfn "%A" str
                    
                


                
            | _,_ -> printfn ("WRONG FORMAT")
            printfn "...Parsing Done"
        | false -> parsing <- false