namespace Tracer.ImplicitSurfaces

module PolyToUnipoly =
  open Tracer.ImplicitSurfaces.ExprToPoly

  // poly with floats, no variables (except the implicit t) to be found around here
  // in other words, its a univariate poly
  type unipoly = UP of Map<int, float>
  type unipoly with
    // TO DO, need to implement this correctly!!
    static member ( % ) (up1:unipoly,up2:unipoly) = up1

  type poly = ExprToPoly.poly
  type simpleExpr = ExprToPoly.simpleExpr

  // only works if all the inner variables are known
  // returns a univariate polynomial, of type unipoly
  let polyToUnipoly (P m:poly) vars =
    UP (Map.fold
          (fun res k v ->
            Map.add k (solveSE vars v) res)
          Map.empty m)
  
  // not sure if this is the fastest way to do it. Whould pattern matching be smarter?
  let rec solveUnipoly (UP m) t = Map.fold (fun acc k v -> acc + (t**(float k) * v)) 0.0 m

  let unipolyDerivative (UP m:unipoly) =
    UP (Map.fold 
          (fun res k v ->
            if k = 0 then res
            else Map.add (k - 1) (v * float k) res)
          Map.empty m)

  // not sure about the return value. I'll figure that out soon, hopefully
  // let's only accept the <int,float> version of poly (i.e. no simpleExpr here pls)
  (*let sturmSeq up : float =
    let up' = unipolyDerivative up
    let rec inner (plist: unipoly list) = 
      let (UP m) = plist.[0]
      match getOrder m with
      | 0 -> failwith "need to figure out what do"
      | 1 -> failwith "need to grow more brain cells"
      | _ -> (plist.[1] % plist.[0]) :: plist

    let plist = inner [up',up] // p0 will always be the last element in the list
    0.0*)
    
  // assuming p2 is of lower order
  // returns a SimpleExpr * (SimpleExpr * SimpleExpr) option, where the last part, the option, is a potential remainder
  let polynomialLongDivision (p1:(int * float) list) (p2:(int * float) list) : (int * float) list * (simpleExpr * simpleExpr) option =
    let (divExp, divConst) = p2.[0]
    
    let p1 = [(3,3.0);(2,-2.0);(1,7.0);(0,-4.)]
    let p2 = [(2,1.0);(0,1.0)]

    let subt (p1:(int * float) list) (p2:(int * float) list) (nn,cc) =
      printfn "%A" (nn,cc)
      let toSubtract = [for (n,c) in p2 do
                          yield (n + nn, c * cc)]
      toSubtract
    //let test = subt [] [(2,1.0);(1,3.0)] (1,1.0)
    let rec inner res = function
      | []        -> res
      | (n,c)::cr -> let currVal = (n-divExp, c / divConst)
                     let resPoly = subt p1 p2 currVal
                     resPoly
    
    let x = inner [] p1
    // HMMM I am a bit unsure if I should go back to Map<int,float>, or Map<int, simpleExpr>, or stay with the current lists
    p1, None

