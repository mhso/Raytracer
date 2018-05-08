namespace Tracer.ImplicitSurfaces

module PolyToUnipoly =
  open Tracer.ImplicitSurfaces.ExprToPoly
  
  type poly = ExprToPoly.poly
  type simpleExpr = ExprToPoly.simpleExpr

  // univariate polynomial type, i.e. only one variable, which is implicitly present in all map elements
  type unipoly = UP of Map<int, float>

  let epsilon = 10.**(-14.) // we consider this to be as good as zero. Might wanna adjust this...

  // only works if the poly terms only consists of ANums
  let polyToUnipoly (P m:poly) vars =
    UP (Map.fold
          (fun res k v ->
            Map.add k (solveSE vars 0.0 v) res)
          Map.empty m)
  
  let rec solveUnipoly (UP m) t = Map.fold (fun acc k v -> if k > 0 then acc + pown t k * v
                                                           else acc + v) 0.0 m

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
                if newC = 0.0 || abs newC < epsilon then Map.remove k res
                else Map.add k newC res
            | None -> Map.add k -v res
       ) m1 m2)

  let getOrder up =
    let (order,_) = getFirstTerm up
    order
  
  let isEmpty (UP m) = m.IsEmpty

  let negateUnipoly (UP m) = UP (Map.fold (fun acc k v -> Map.add k -v acc) Map.empty m)

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
        if isEmpty f' then (q, f)
        else inner f' s q'
    inner up1 up2 (UP Map.empty)

  type unipoly with 
    static member ( % ) (up1, up2) = unipolyLongDiv up1 up2

  // not sure about the return value. I'll figure that out soon, hopefully
  // let's only accept the <int,float> version of poly (i.e. no simpleExpr here pls)
  let sturmSeq up up' : unipoly list =
    let rec inner (uplist: unipoly list) = 
      match getOrder uplist.[0] with
      | 0 -> uplist
      | _ -> let (_,res) = uplist.[1] % uplist.[0]
             inner ((negateUnipoly res) :: uplist)
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
    let roots g1 g2 =  
      let s1 = countSignChanges uplist g1
      let s2 = countSignChanges uplist g2
      s1 - s2
    let rec binarySearch lo hi fmr count =
      if count < 1 then Some ((hi - lo) / 2.0)
      else if hi < lo then None
      else
        let mid = (lo + hi) / 2.0
        let dif = roots lo hi
        match dif with
        | 1 -> Some ((hi - lo) / 2.0)
        | 0 -> binarySearch mid fmr fmr (count - 1)
        | _ -> binarySearch lo mid hi (count - 1)

    binarySearch 0.0 100.0 0.0 50