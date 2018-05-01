module PLYParser

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
    | Success(_) ->  true
    | Failure (_) ->  false

let WhiteSpace = pstring " "

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
        let result = parseBool (pstring "ply") (nextLine)
        match result with

        | true -> 
            printfn "Started Parsing..."

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
            let triangleArray = Array.zeroCreate arraySize
            let isEndOfHeader s = parseBool (pstring "end_header") s
            let vertexPosition : int array = Array.zeroCreate 9
            let readPropertyParser = 
                nextLine <- sr.ReadLine()
                let startWithProperty (s:string) = parseBool (pstring "property" .>> (anyString (s.Length-8))) s
                while (startWithProperty nextLine) do
                    let endChars = nextLine.Substring(nextLine.Length-2)
                    match endChars with
                        | " x" -> vertexPosition.[0] <- numberOfProperty
                        | " y" -> vertexPosition.[1] <- numberOfProperty
                        | " z" -> vertexPosition.[2] <- numberOfProperty
                        | "nx" -> vertexPosition.[3] <- numberOfProperty
                        | "ny" -> vertexPosition.[4] <- numberOfProperty
                        | "nz" -> vertexPosition.[5] <- numberOfProperty
                        | " u" -> vertexPosition.[6] <- numberOfProperty
                        | " v" -> vertexPosition.[7] <- numberOfProperty
                        | _ -> vertexPosition.[8] <- numberOfProperty
                    nextLine <- sr.ReadLine()
                    numberOfProperty <- numberOfProperty + 1
            readPropertyParser

            let faceLength = parse (pstring "element face " >>. pint32) nextLine
            let faceArray = Array.zeroCreate faceLength
            nextLine <- sr.ReadLine()
            while (not (isEndOfHeader nextLine)) do
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
                for j in 0..(triangleArray.Length-1) do 
                    let vertexProps = Array.zeroCreate(numberOfProperty-1)
                    for k in 0..numberOfProperty-2 do
                        let buffer : byte[] = Array.zeroCreate(4)
                        br.Read(buffer,0,4)
                        if(isBigEndian) then
                            Array.Reverse(buffer)
                        let f = BitConverter.ToSingle(buffer, 0)
                        vertexProps.[k] <- (float (f))
                    triangleArray.[j] <- findVertexFromArray (vertexProps |> Array.toList) vertexPosition
                for j in 0..(faceArray.Length-1) do 
                    let length : byte[] = Array.zeroCreate(13)
                    br.Read(length, 0,13)
                    faceArray.[j] <- [BitConverter.ToInt32(length,0); BitConverter.ToInt32(length,1);BitConverter.ToInt32(length,5);BitConverter.ToInt32(length,9)]
            | _,_ -> failwith ("Parsing Error: TAMPERED PLY FILE")
            printfn "...Parsing Done"
        | false -> parsing <- false