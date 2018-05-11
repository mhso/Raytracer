namespace Tracer.ImplicitSurfaces

module PolyToUnipoly =
  open Tracer.ImplicitSurfaces.ExprToPoly
  
  type poly = ExprToPoly.poly
  type simpleExpr = ExprToPoly.simpleExpr

  // univariate polynomial type, i.e. only one variable, which is implicitly present in all map elements
  type unipoly = UP of (int * float) list

  let epsilon = 10.**(-14.) // we consider this to be as good as zero. Might wanna adjust this...

  let polyToUnipoly (p: (int*simpleExpr) list) ox oy oz dx dy dz : unipoly =
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
        pown v x
    let agsolver ag = List.fold (fun acc a -> acc * asolver a) 1.0 ag
    let sesolver se = List.fold (fun acc ag -> acc + agsolver ag) 0.0 se
    UP (List.fold (fun acc (n, (SE se)) -> (n, sesolver se)::acc) [] p)

  let solveUnipoly (UP up:unipoly) t =
    List.fold (fun acc (n,c) -> if n > 0 then (acc + (pown t n) * c)
                                else (acc + c)) 0.0 up

  let unipolyDerivative (UP up:unipoly) : unipoly =
    UP (List.foldBack (fun (n,c) acc -> 
                     if n = 0 then acc
                     else (n-1, float n * c) :: acc) up [])

  let getFirstTerm (UP up:unipoly) = up.[0]

  let multUnipoly (UP up:unipoly) (exp, con) : unipoly =
    UP (List.foldBack (fun (n,c) acc -> (n + exp, c * con)::acc) up [])

  let negateUnipoly (UP up:unipoly) = UP (List.foldBack (fun (n,c) acc -> (n, -c)::acc) up [])

  // up2 is the unipoly to be subtracted from up1
  let subtractUnipoly (UP up1:unipoly) (UP up2:unipoly) : unipoly =
    let rec inner res sub = function
    | []        ->  if List.length sub > 0 then 
                      let (UP rest) = negateUnipoly (UP sub)
                      res @ rest
                    else res
    | (n,c)::cr ->  if sub.Length < 1 then
                      if abs c < epsilon then inner res sub cr
                      else inner (res @ [(n, c)]) sub cr
                    else 
                      let sn,sc = sub.[0]
                      if sn = n then
                        let v = c - sc
                        if abs v < epsilon then inner res sub.Tail cr
                        else inner (res @ [(n, v)]) sub.Tail cr
                      else if sn < n then inner (res @ [(n,c)]) sub cr
                      else inner (res @ [(sn, -sc)]) sub.Tail ((n,c)::cr)
    UP (inner [] up2 up1)

  let getDegree (UP up:unipoly) =
    let (degree,_) = up.[0]
    degree
  
  let isEmpty (UP up:unipoly) = up.IsEmpty

  type unipoly with
    static member ( - ) (up1, up2)    = subtractUnipoly up1 up2
    static member ( * ) (up1, (n, c)) = multUnipoly up1 (n, c)

  (*
    Assuming up2 is of lower order than up1
    Returns the remainder of the polynomial long division
    Logic inspired from https://rosettacode.org/wiki/Polynomial_long_division#OCaml
  *)
  let unipolyLongDiv up1 up2 : unipoly * unipoly =
    // s is the smaller, f the larger, q is quotient.
    let rec inner (f:unipoly) (s:unipoly) (UP q:unipoly) =
      let ddif = getDegree f - getDegree s
      if ddif < 0 then (UP q, f)
      else
        let (fExp, fConst) = getFirstTerm f // dividend's highest degree term
        let (sExp, sConst) = getFirstTerm s // divisor's highest degree term
        let k = fExp - sExp, fConst / sConst // division of the two terms
        let ks = s * k // k multiplied into the divisor poly
        let q' = UP (q @ [k]) // k added to the current result from former iterations
        let f' = f - ks
        if isEmpty f' then (UP q, f)
        else inner f' s q'
    inner up1 up2 (UP [])

  type unipoly with 
    static member ( % ) (up1, up2) = unipolyLongDiv up1 up2

  // not sure about the return value. I'll figure that out soon, hopefully
  // let's only accept the <int,float> version of poly (i.e. no simpleExpr here pls)
  let sturmSeq up up' : unipoly list =
    let rec inner (uplist: unipoly list) = 
      match getDegree uplist.[0] with
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