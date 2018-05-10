namespace Tracer.ImplicitSurfaces

module ExprToPoly =

  open Tracer.ImplicitSurfaces.ExprParse

  type expr = Tracer.ImplicitSurfaces.ExprParse.expr
  
  let rec ppExpr = function
    | FNum c            -> string(c)
    | FVar s            -> s
    | FAdd(e1,e2)       -> "(" + (ppExpr e1) + " + " + (ppExpr e2) + ")"
    | FMult(e1,e2)      -> (ppExpr e1) + " * " + (ppExpr e2)
    | FExponent(e,n)    -> "(" + (ppExpr e) + ")^" + string(n)
    | FDiv(e1,e2)       -> ppExpr e1 + " / " + ppExpr e2
    | FRoot(e,n)        -> "(" + ppExpr e + ")_" + string(n)

  let rec subst e (x,ex) =
    match e with    
    | FNum c          -> FNum c
    | FVar s          -> if s = x then ex else e
    | FAdd(a,b)       -> FAdd(subst a (x,ex), subst b (x, ex))
    | FMult(a,b)      -> FMult(subst a (x,ex), subst b (x,ex))
    | FExponent(a,i)  -> FExponent(subst a (x,ex), i)
    | FDiv(a,b)       -> FDiv(subst a (x,ex), subst b (x,ex))
    | FRoot(a,i)      -> FRoot(subst a (x,ex), i)

  type atom = ANum of float | AExponent of string * int | ARadical of simpleExpr * int
  and atomGroup = atom list  
  and simpleExpr = SE of atomGroup list

  let isSimpleExprEmpty (SE ags) = List.isEmpty ags || ags = [[]]

  let ppAtom = function
    | ANum c          -> string(c)
    | AExponent(s,1)  -> s
    | AExponent(s,n)  -> s+"^"+(string(n))
    | _               -> ""
  let ppAtomGroup ag = String.concat "*" (List.map ppAtom ag)
  let ppSimpleExpr (SE ags) = String.concat "+" (List.map ppAtomGroup ags)

  let rec combine (xss:atomGroup list) = function
    | [] -> []
    | ys::yss -> List.map ((@) ys) xss @ combine xss yss

  // Invoking the spirit of Muhammad ibn Musa al-Khwarizmi
  let rec simplify = function
    | FNum c          -> [[ANum c]]
    | FVar s          -> [[AExponent(s,1)]]
    | FRoot(e1,n)     -> [[ARadical(SE (simplify e1),n)]]
    | FAdd(e1,e2)     -> simplify e1 @ simplify e2
    | FMult(e1,e2)    -> combine (simplify e1) (simplify e2)
    | FDiv(e1,e2)     -> combine (simplify e1) (simplify (FExponent(e2, -1))) // e1 / e2 is the same as e1 * e2^-1 (because e2^-1 = 1 / e2)
    | FExponent(_,0)  -> simplify (FNum 1.0)
    | FExponent(e1,1) -> simplify e1
    | FExponent(e1,n) -> if n < 0 then 
                            match e1 with
                            | FNum c  -> combine (simplify (FNum (1./c))) (simplify (FExponent(e1, n + 1)))
                            | FVar s1 -> if n = -1 then [[AExponent(s1,-1)]]
                                         else combine [[AExponent(s1, -1)]] (simplify (FExponent(e1, n + 1)))
                            | _       -> failwith "simplify: unmatched expr" // TODO: I need to figure out what to do when we encounter other stuff
                         else combine (simplify e1) (simplify (FExponent(e1, n-1)))

  let rec highestRoot (c:int) = function
    | []      -> c
    | ag::cr  -> 
        highestRoot (List.fold (fun c x -> 
          match x with
          | ANum _          -> c
          | AExponent _     -> c
          | ARadical(_,n)   -> (max c n) ) c ag) cr
                            
  let rec containsRoots s =
    highestRoot 0 s > 0

  let rec simplifyExpr e =
    let rec inner ex =
      match ex with
      // numbers
      | FAdd(FNum c1, FNum c2)  -> FNum (c1 + c2)
      | FMult(FNum c1, FNum c2) -> FNum (c1 * c2)
      | FDiv(FNum c1, FNum c2)  -> FNum (c1 / c2)
      | FExponent(FNum c, n)    -> FNum (c**(float n))
      | FRoot(FNum c, n)        -> FNum (c**(1./(float n)))
      // exponents
      | FExponent(_,0)          -> FNum 1.0
      | FExponent(e1,1)         -> simplifyExpr e1
      // division cases
      | FMult(e1, FDiv(e2, e3)) -> FDiv(FMult(inner e1, inner e2), inner e3) // case 6
      | FMult(FDiv(e2, e3), e1) -> FDiv(FMult(inner e1, inner e2), inner e3) // case 6
      | FDiv(FDiv(e1, e2), FDiv(e3, e4)) -> FDiv(FMult(inner e1, inner e4), FMult(inner e2, inner e3)) // case 10
      | FDiv(FDiv(e1, e2), e3)  -> FDiv(inner e1, FMult (inner e2, inner e3)) // case 7
      | FDiv(e1, FDiv(e2, e3))  -> FDiv(FMult(inner e1, inner e3), inner e2) // case 8
      | FAdd(e1, FDiv(e2, e3))  -> FDiv(FAdd(FMult(inner e1, inner e3), inner e2), inner e3) // case 9
      | FAdd(FDiv(e2, e3), e1)  -> FDiv(FAdd(FMult(inner e1, inner e3), inner e2), inner e3) // case 9
      // all others, just go down recursively, with no changes at this level
      | FAdd(e1,e2)             -> FAdd(inner e1, inner e2)
      | FMult(e1, e2)           -> FMult(inner e1, inner e2)
      | FRoot(e1, n)            -> FRoot(inner e1, n)
      | FExponent(e1, n)        -> FExponent(inner e1,n)
      | FDiv(e1, e2)            -> FDiv(inner e1, inner e2)
      | _                       -> ex // FVar and FNum
    let rewritten = inner e
    if rewritten = e then e
    else simplifyExpr rewritten

  let removeNRoots s =
    let rec inner ag =
      let mutable roots = Map.empty
      let mutable freed = [[]]
      let mutable rest = [[]]
      for a in ag do
        match a with
          | ANum _      -> rest <- combine [[a]] rest
          | AExponent _ -> rest <- combine [[a]] rest
          | ARadical((SE x),n)  ->
            match Map.tryFind a roots with
            | Some v  -> if n = (v + 1) 
                         then freed <- combine x freed
                              roots <- Map.remove a roots
                         else roots <- Map.add a (v + 1) roots
            | None    -> roots <- Map.add a 1 roots
      for KeyValue(k,v) in roots do let remainingRoots = [for _ in 1 .. v -> [[k]]] |> List.fold (combine) [[]]
                                    rest <- combine rest remainingRoots
      combine rest freed
    List.fold (fun acc x -> acc @ (inner x)) [] s

  let rec simplifyRoots s =
    let s' =  removeNRoots s // e_2 * e_2 will be e
    let rec inner nr r = function
      | []      -> nr, r
      | ag::cr  -> if containsRoots [ag] then inner nr (r @ [ag]) cr
                     else inner (nr @ [ag]) r cr
    let (noroots, roots) = inner [] [] s'
    if roots <> [] then
      let k = highestRoot 0 roots // first we find the highest root we want to get rid of
      
      // no roots term. Multiply by -1, then to the power of k, and then again multiply by -1
      let nrTerm = combine [[ANum -1.0]] noroots
      let nrTermMultiplied = [for _ in 1 .. k -> nrTerm] |> List.fold (combine) [[]]
      let nrTermDone = combine [[ANum -1.0]] nrTermMultiplied
      
      let rtTerm = [for _ in 1 .. k -> roots] |> List.fold (combine) [[]]
      let result = nrTermDone @ removeNRoots rtTerm

      if containsRoots result then simplifyRoots result else result
    else noroots

  let simplifyAtomGroup ag : atomGroup =
    let mutable nums = 1.0
    let mutable exps = Map.empty
    for a in ag do
      match a with
        | ANum n          -> nums <- n * nums
        | AExponent(x,n)  -> 
          match Map.tryFind x exps with
            | Some v  -> exps <- Map.add x (n + v) exps
            | None    -> exps <- Map.add x n exps
        | _ -> failwith "should not end here"
    let expslist = [for KeyValue(k,v) in exps ->
                        if v = 0 then ANum 1.0
                        else AExponent(k,v)]
    if nums = 0.0 then []
    else [ANum nums] @ expslist

  let simplifySimpleExpr (SE ags) =
    let ags' = List.map simplifyAtomGroup ags
    let mutable nums = 0.0
    let mutable vars = Map.empty
    List.iter (fun x ->
      match x with
      | [ANum n]   -> nums <- n + nums // Add atom groups with only constants together.
      | ANum n::cr -> 
            match Map.tryFind cr vars with
            | Some v  -> vars <- Map.add cr (v + n) vars
            | None    -> vars <- Map.add cr n vars
      | []            -> nums <- 0.0 
      | _             -> failwith "simplifySimpleExpr: unmatched clause" // should never get here                   
    ) ags'
    // Last task is to group similar atomGroups into one group.
    let varslist = [for KeyValue(k,v) in vars -> if v = 1.0 then k
                                                 //else if v = 0.0 then [ANum 0.0]
                                                 else ANum v::k]
    if nums <> 0.0 then SE ([[ANum nums]] @ varslist)
    else SE varslist

  let rewriteExpr e =
    let reduced =
      match simplifyExpr e with
      | FDiv(keep,_)  -> keep
      | _             -> e
    (simplifyRoots << simplify) reduced

  let exprToSimpleExpr (e:expr) :simpleExpr = simplifySimpleExpr (SE (rewriteExpr e)) // swapped simplify with rewriteExpr

  type poly = P of Map<int,simpleExpr>

  let ppPoly v (P p) =
    let pp (d,ags) =
      let prefix = if d=0 then "" else ppAtom (AExponent(v,d))
      let postfix = if isSimpleExprEmpty ags then "" else "(" + (ppSimpleExpr ags) + ")"
      prefix + postfix
    String.concat "+" (List.map pp (Map.toList p))

  // Collect atom groups into groups with respect to one variable v
  let splitAG v m = function
    | [] -> m
    | ag ->
      let eqV = function 
        | AExponent(v',_) -> v = v' 
        | _               -> false
      let addMap d ag m = 
        match Map.tryFind d m with
        | Some (SE alist) -> Map.add d (SE (ag::alist)) m
        | None            -> Map.add d (SE [ag]) m
      match List.tryFind eqV ag with
        | Some (AExponent(_,d)) ->
          let ag' = List.filter (not << eqV) ag
          addMap d ag' m
        | Some _  -> failwith "splitAG: Must never come here! - ANum will not match eqV"
        | None    -> addMap 0 ag m

  let simpleExprToPoly (SE ags) v =
    P (List.fold (splitAG v) Map.empty ags)

  // same as (simpleExprToPoly (simplifySimpleExpr (exprToSimpleExpr e)) v)
  let exprToPoly e v = (exprToSimpleExpr >> simplifySimpleExpr >> simpleExprToPoly) e v

  // derivative of a polynomial, with respect to t
  let polyDerivative (P m) = 
    let rec inner m' = function
    | []    -> m'
    | t::cr -> 
        match t with
        | (0,_)       -> inner m' cr
        | (n,(SE s))  -> 
              let updated = 
                Map.add
                  (n-1) 
                  (simplifySimpleExpr (SE (combine s [[ANum (float n)]]) ))
                  m'
              inner updated cr
    P (inner Map.empty (Map.toList m))

  let rec solveAG m = function
    | []   -> 1.0
    | a::r -> 
        match a with
        | ANum c         -> c * solveAG m r
        | AExponent(e,x) -> 
            match Map.tryFind e m with
            | Some v -> if x = 1 then v * solveAG m r
                          else v**(float x) * solveAG m r
            | None   -> failwith "solveAG: variable not found in map"
        | _ -> failwith "solveAG: met an atom that shouldn't exist here"                
  
  // maps in fsharp are ordered according to generic comparison, and since our keys are ints,
  // we know that the last element will have the largest integer value
  // the following should then be faster than iterating over all the elements
  let getOrder (m:Map<int,'a>) = (m |> Seq.last).Key
  
  // requires a map of all variables, mapped to float values
  let rec solveSE m acc = function
    | SE ([])     -> acc
    | SE (ag::cr) -> solveSE m (acc + solveAG m ag) (SE cr)

  let toList (P m:poly) = Map.toList m
