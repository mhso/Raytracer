module PLYParser

open FParsec
open System.IO

type UserState = unit
type Parser<'t> = Parser<'t,UserState>
type Vertex(x,y,z) = 
    member this.x = x
    member this.y = y
    member this.z = z


let parse parser str = 
    match run parser str with
    | Success(result,_,_) ->  result
    | Failure (errorMsg,_,_) ->  failwith("WRONG FORMAT")

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
                let readPropertyParser = 
                    nextLine <- sr.ReadLine()
                    let startWithProperty (s:string) = parseBool (pstring "property" .>> (anyString (s.Length-8))) s
                    while (startWithProperty nextLine) do
                        printfn "is a property tag"
                        nextLine <- sr.ReadLine()
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