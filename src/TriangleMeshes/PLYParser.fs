module PLYParser

open FParsec
open System.IO

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

let parsePLY (filepath:string) = 
    let sr = new StreamReader(filepath)
    let mutable parsing = true
    if (not sr.EndOfStream) then
        let mutable nextLine = sr.ReadLine()
        let result = parseBool (pstring "ply") (nextLine)
        match result with
        | true -> 
            nextLine <- sr.ReadLine()
            let isAscii = parseBool (pstring "format ascii 1.0") (nextLine)
            let formatBoolean = many ((pstring "format binary_little_endian 1.0") <|> (pstring "format binary_big_endian 1.0"))
            let isBoolean = parseBool (formatBoolean) (nextLine)
            match isAscii,isBoolean with
            | true,_ -> 
                nextLine <- sr.ReadLine()
                let isComment (s:string) = parseBool (pstring "comment" .>> (anyString (s.Length-7))) s
                while (isComment nextLine) do 
                    printfn "%A" nextLine
                    nextLine <- sr.ReadLine()
                let arraySizeParser = pstring "element vertex " >>. pint32
                let arraySize = parse arraySizeParser nextLine
                let triangleArray = Array.zeroCreate arraySize
                let isEndOfHeader s = parseBool (pstring "end_header") s
                let vertexPosition : int array = Array.zeroCreate 8
                let readPropertyParser = 
                    let mutable i = 0
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
                            | _ -> printfn "not used propertyTag"
                        nextLine <- sr.ReadLine()
                        i <- i + 1
                readPropertyParser
                let faceLength = parse (pstring "element face " >>. pint32) nextLine
                let faceArray = Array.zeroCreate faceLength
                nextLine <- sr.ReadLine()
                while (not (isEndOfHeader nextLine)) do
                    //let isFaceValue = parseBool (pstring "element face" >> (anyString (nextLine.Length-12))) nextLine
                    printfn "Final Property values"
                    nextLine <- sr.ReadLine()
                for i in 0..triangleArray.Length-1 do
                    nextLine <- nextLine.Substring(0,nextLine.Length-1)
                    let listFloatParser = (sepBy pfloat WhiteSpace)
                    let listFloat = parse listFloatParser nextLine
                    //let findVertex (a: int array) (b : float list)= 
                    //    printfn "%A" a
                        
                    //    new Vertex (b.[a.[0]], b.[a.[1]], b.[a.[2]], b.[a.[3]], b.[a.[4]], b.[a.[5]], b.[a.[6]], b.[a.[7]])
                    triangleArray.[i] <- listFloat
                    nextLine <- sr.ReadLine()
                printfn "Array Of Vertices done"
                for i in 0..faceArray.Length-1 do 
                    nextLine <- nextLine.Substring(0,nextLine.Length-1)
                    let listIntParser = (sepBy pint32 WhiteSpace)
                    let listInt = parse listIntParser nextLine
                    faceArray.[i] <- listInt
                    nextLine <- sr.ReadLine()
                printfn "Array Of Face done"
                //let triangleParse = pint32 >> WhiteSpace >>. sepBy pint32 WhiteSpace
            | _,true -> printfn ("BINARY START")
            | _,_ -> printfn ("WRONG FORMAT")

        | false -> parsing <- false