namespace Tracer.ImplicitSurfaces

module PolyToUnipoly =
  open Tracer.ImplicitSurfaces.ExprToPoly
  
  type poly = ExprToPoly.poly
  type simpleExpr = ExprToPoly.simpleExpr

  // univariate polynomial type, i.e. only one variable, which is implicitly present in all map elements
  type unipoly = (int * float) list

  let epsilon = 10.**(-14.) // we consider this to be as good as zero. Might wanna adjust this...

  let polyToUnipoly (p: (int*simpleExpr) list) ox oy oz dx dy dz =
    let asolver = function
    | ANum c         -> c
    | AExponent(e,x) -> 
        let v =
          match e with
          | "ox" -> ox
          | "oy" -> oy
          | "oz" -> oz
          | "dx" -> dx
          | "dy" -> dy
          | "dz" -> dz
          | _    -> failwith "polyToUnipoly: unmatched clause"
        if x = 1 then v
        else pown v x
    let agsolver ag = List.fold (fun acc a -> acc * asolver a) 0.0 ag
    let sesolver se = List.fold (fun acc ag -> acc + agsolver ag) 0.0 se
    List.fold (fun acc (n, (SE se)) -> (n, sesolver se)::acc) [] p
  
  let solveUnipoly up t =
    List.fold (fun acc (n,c) -> 
                  if n > 0 then (acc + pown t n * c)
                  else (acc + c)) 0.0 up

  let unipolyDerivative up =
    List.foldBack (fun (n,c) acc -> 
                     if n = 0 then acc
                     else (n-1, float n * c) :: acc) up []

  // Now I need to check what is right...
  let getFirstTerm (up:unipoly) = up.[0]

  let multUnipoly up (exp, con) =
    List.foldBack (fun (n,c) acc -> (n + exp, c * con) :: acc) up []

  let subtractUnipoly up1 up2 =
    UP (Map.fold (
          fun res k v ->
            let newval = match Map.tryFind k res with
                         | Some mval -> mval-v
                         | None      -> -v
            if newval = 0.0 || abs newval < epsilon then Map.remove k res
            else Map.add k newval res
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

  let getInterval uplist intvallo intvalhi maxdepth =
    let roots g1 g2 =  
      let s1 = countSignChanges uplist g1
      let s2 = countSignChanges uplist g2
      s1 - s2
    let rec search lo hi currentdepth =
      let mid = (lo + hi) / 2.
      if currentdepth >= maxdepth then Some (lo, hi, mid)
      else
        let lodiff = roots lo mid
        if lodiff > 0 then search lo mid (currentdepth + 1)
        else
          let hidiff = roots mid hi
          if hidiff > 0 then search mid hi (currentdepth + 1)
          else None
    search intvallo intvalhi 0