namespace Tracer.ImplicitSurfaces

module PolyToUnipoly =
  open Tracer.ImplicitSurfaces.ExprToPoly
  
  type poly = ExprToPoly.poly
  type simpleExpr = ExprToPoly.simpleExpr

  // univariate polynomial type, i.e. only one variable, which is implicitly present in all map elements
  type unipoly = UP of Map<int, float>

  // only works if the poly terms only consists of ANums
  let polyToUnipoly (P m:poly) vars =
    UP (Map.fold
          (fun res k v ->
            Map.add k (solveSE vars v) res)
          Map.empty m)
  
  let rec solveUnipoly (UP m) t = Map.fold (fun acc k v -> acc + (t**(float k) * v)) 0.0 m

  let unipolyDerivative (UP m:unipoly) =
    UP (Map.fold 
          (fun res k v ->
            if k = 0 then res
            else Map.add (k - 1) (v * float k) res)
          Map.empty m)

  let getFirstTerm (UP m) =
    let kvp = m |> Seq.last
    (kvp.Key, kvp.Value)

  let multUnipoly (UP m) (exp, con) =
    UP (Map.fold (
          fun res k v ->
            Map.add (k + exp) (v * con) res
        ) Map.empty m)

  let subtractUnipoly (UP m1) (UP m2) =
    UP (Map.fold (
          fun res k v ->
            match Map.tryFind k res with
            | Some m1val -> 
                let newC = m1val - v
                if newC = 0.0 then Map.remove k res
                else Map.add k newC res
            | None -> Map.add k -v res
       ) m1 m2)

  let getOrder up =
    let (order,_) = getFirstTerm up
    order

  type unipoly with
    static member ( - ) (up1, up2)    = subtractUnipoly up1 up2
    static member ( * ) (up1, (n, c)) = multUnipoly up1 (n, c)

  (*
    Assuming up2 is of lower order than up1
    Returns the remainder of the polynomial long division
  *)
  let unipolyLongDiv up1 up2 : unipoly * unipoly =
    // s is the smaller, f the larger, q is quotient.
    let rec inner (f:unipoly) (s:unipoly) (q:unipoly) =
      let ddif = getOrder f - getOrder s
      if ddif < 0 then (q, f)
      else
        let (fExp, fConst) = getFirstTerm f
        let (sExp, sConst) = getFirstTerm s
        let k = fExp - sExp, fConst / sConst
        let ks = s * k
        let (UP qmap) = q
        let q' = UP (Map.add (fExp - sExp) (fConst / sConst) qmap)
        let f' = f - ks
        inner f' s q'
    inner up1 up2 (UP Map.empty)

  type unipoly with 
    static member ( % ) (up1, up2) = unipolyLongDiv up1 up2

  // not sure about the return value. I'll figure that out soon, hopefully
  // let's only accept the <int,float> version of poly (i.e. no simpleExpr here pls)
  let sturmSeq up : unipoly list =
    let up' = unipolyDerivative up
    let rec inner (uplist: unipoly list) = 
      match getOrder uplist.[0] with
      | 0 -> uplist
      | 1 -> uplist
      | _ -> let (_,res) = uplist.[1] % uplist.[0]
             inner (res :: uplist)
    inner [up';up] // p0 will always be the last element in the list
  
  let countSignChanges uplist x =
    let rec inner fmr cnt = function
    | []        -> cnt
    | up::rest  ->
        let rs = solveUnipoly up x
        if (rs > 0.0 && fmr > 0.0) || (rs < 0.0 && fmr < 0.0)
          then inner rs cnt rest
        else inner rs (cnt + 1) rest
    inner 0.0 0 uplist

  let makeGuess uplist =
    let lox = 0.0
    let lo = countSignChanges uplist lox
    let rec inner hix fmr =
      let hi = countSignChanges uplist hix
      let diff = lo - hi
      match diff with
      | 1             -> Some hix
      | c when c < 1  -> 
          if fmr > 1 then inner (hix * 1.8) diff
          else None
      | _             -> 
          if hix < 1.0 then None
          else 
            printfn "hix: %f, and diff: %i" hix diff
            inner (hix / 2.0) diff
    inner 6.672 0