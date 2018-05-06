﻿namespace Tracer.ImplicitSurfaces

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
    let rec aux (f:unipoly) (s:unipoly) (UP q:unipoly) =
      let ddif = getOrder f - getOrder s
      if ddif < 0 then (UP q, f)
      else
        let (fExp, fConst) = getFirstTerm f
        let (sExp, sConst) = getFirstTerm s
        let k = fExp - sExp, fConst / sConst
        let ks = s * k
        let q' = Map.add (fExp - sExp) (fConst / sConst) q
        let f' = f - ks
        aux f' s (UP q')
    aux up1 up2 (UP Map.empty)

  type unipoly with 
    static member ( % ) (up1, up2) = unipolyLongDiv up1 up2

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
    

