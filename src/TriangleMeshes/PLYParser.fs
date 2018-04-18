module PLYParser

open FParsec

type UserState = unit
type Parser<'t> = Parser<'t,UserState>

let parse parser str = 
    match run parser str with
    | Success(result,_,_) ->  result
    | Failure (errorMsg,_,_) ->  failwith("WRONG FORMAT")

//let test = parseFloat pfloat "2.25"
//let example = test * 2.

let parseString s = pstring s
let parseMultipleFloatsOneLine : Parser<_> = parseString "[" >>. sepBy pfloat (parseString ";") .>> parseString "]"

//let str s = pstring s
//let betweenStrings pBegin pEnd parser = pBegin >>. parser .>> pEnd

let stringToFloatList : Parser<_> = (sepBy pfloat (parseString " "))
let stringToFloatListList : Parser<_> = sepBy (sepBy pfloat (parseString " ")) (parseString ("\n"))


//let parseString = sepBy pstring (parseString " ")

