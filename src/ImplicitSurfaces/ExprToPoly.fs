namespace Tracer.ImplicitSurfaces

open System.Xml.Linq
module ExprToPoly =

  open Tracer.ImplicitSurfaces.ExprParse
  
  type expr = ExprParse.expr

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

  type atom = ANum of float | AExponent of string * int
  and atomGroup = atom list  
  and simpleExpr = SE of atomGroup list

  let isSimpleExprEmpty (SE ags) = List.isEmpty ags || ags = [[]]

  let ppAtom = function
    | ANum c          -> string(c)
    | AExponent(s,1)  -> s
    | AExponent(s,n)  -> s+"^"+(string(n))
  let ppAtomGroup ag = String.concat "*" (List.map ppAtom ag)
  let ppSimpleExpr (SE ags) = String.concat "+" (List.map ppAtomGroup ags)

  // xss is multiplied on all the ys's that are found. At least that's what I think is happening.
  let rec combine (xss:atomGroup list) = function
    | [] -> []
    | ys::yss -> List.map ((@) ys) xss @ combine xss yss

  // Invoking the spirit of Muhammad ibn Musa al-Khwarizmi
  let rec simplify = function
  | FNum c            -> [[ANum c]]
  | FVar s            -> [[AExponent(s,1)]]
  | FRoot(e1,n)       -> failwith "shoot me now"
  | FAdd(e1,FNum 0.0) -> simplify e1
  | FAdd(FNum 0.0,e1) -> simplify e1
  | FAdd(e1,e2)       -> simplify e1 @ simplify e2
  | FMult(e1,e2)      -> combine (simplify e1) (simplify e2)
  | FDiv(e1,e2)       -> combine (simplify e1) (simplify (FExponent(e2, -1))) // e1 / e2 is the same e1 * e2^-1 (because e2^-1 = 1 / e2)
  | FExponent(_,0)    -> [[ANum 1.0]]
  | FExponent(e1,1)   -> simplify e1
  | FExponent(e1,n)   -> if n < 0 then 
                            match e1 with
                            | FNum c  -> combine [[ANum (1./c)]] (simplify (FExponent(e1, n + 1)))
                            | FVar s1 -> if n = -1 then [[AExponent(s1,-1)]]
                                         else combine [[AExponent(s1, -1)]] (simplify (FExponent(e1, n + 1)))
                            | _       -> failwith "simplify: unmatched expr" // TODO: I need to figure out what to do when we encounter other stuff
                         else combine (simplify e1) (simplify (FExponent(e1, n-1)))

  // the following two functions looks so much alike, that I think I maybe could be able to make one function, and apply different active patterns on them?
  let rec containsDiv = function
    | FVar _          -> false
    | FNum _          -> false
    | FDiv _          -> true
    | FAdd(e1,e2)     -> containsDiv e1 || containsDiv e2
    | FMult(e1,e2)    -> containsDiv e1 || containsDiv e2
    | FExponent(e,_)  -> containsDiv e
    | FRoot(e,_)      -> containsDiv e

  let rec containsRadicals = function
    | FVar _          -> false
    | FNum _          -> false
    | FRoot _         -> true
    | FDiv(e1,e2)     -> containsRadicals e1 || containsRadicals e2
    | FAdd(e1,e2)     -> containsRadicals e1 || containsRadicals e2
    | FMult(e1,e2)    -> containsRadicals e1 || containsRadicals e2
    | FExponent(e,_)  -> containsRadicals e

  let rec highestRoot c = function
    | FVar _          -> c
    | FNum _          -> c
    | FRoot(e,n)      -> highestRoot (max c n) e
    | FDiv(e1,e2)     -> let c =highestRoot c e1
                         highestRoot c e2
    | FAdd(e1,e2)     -> let c = highestRoot c e1
                         highestRoot c e2
    | FMult(e1,e2)    -> let c = highestRoot c e1
                         highestRoot c e2
    | FExponent(e,_)  -> highestRoot c e

  let rec simplifyDiv e = 
    let rec inner ex =
      match ex with
      // case 6:
      | FMult(e1, FDiv(e2, e3)) -> FDiv(FMult(inner e1, inner e2), inner e3)
      | FMult(FDiv(e2, e3), e1) -> FDiv(FMult(inner e1, inner e2), inner e3)
      // case 10
      | FDiv(FDiv(e1, e2), FDiv(e3, e4)) -> FDiv(FMult(inner e1, inner e4), FMult(inner e2, inner e3))
      // case 7:
      | FDiv(FDiv(e1, e2), e3)  -> FDiv(inner e1, FMult (inner e2, inner e3))
      // case 8:
      | FDiv(e1, FDiv(e2, e3))  -> FDiv(FMult(inner e1, inner e3), inner e2)
      // case 9:
      | FAdd(e1, FDiv(e2, e3))  -> FDiv(FAdd(FMult(inner e1, inner e3), inner e2), inner e3)
      | FAdd(FDiv(e2, e3), e1)  -> FDiv(FAdd(FMult(inner e1, inner e3), inner e2), inner e3)
      // all others, just go down recursively, with no changes at this level
      | FAdd(e1,e2)             -> FAdd(inner e1, inner e2)
      | FMult(e1, e2)           -> FMult(inner e1, inner e2)
      | FRoot(e1, n)            -> FRoot(inner e1, n)
      | FExponent(e1, n)        -> FExponent(inner e1,n)
      | FDiv(e1, e2)            -> FDiv(inner e1, inner e2)
      | _                       -> ex // FVar and FNum
    let rewritten = inner e
    if rewritten = e then e
    else simplifyDiv rewritten
  
  let rec simplifyRadicals e =
    let rec inner norad rad = function
      | FAdd(e1,e2) ->
          let norad, rad = inner norad rad e1
          let norad, rad = inner norad rad e2
          norad, rad
      | ex  -> 
          if containsRadicals ex then norad, FAdd(rad, ex)
          else FAdd(norad,ex), rad
    let (norad,rad) = inner (FNum 0.0) (FNum 0.0) e
    if rad <> FNum 0.0 then 
      let k = highestRoot 0 rad
      // idea, call simplify on the norads, makes it easier to deal with
      // for norad: create a function that flips positive terms to negative and vice versa (the ecquivalent of moving them from one side of the equation sign to the other)
      // then put both rad and norad in exponent e, k (or combine (combine e e) e)
      // simplify if possible, and for norad toggle again
      // is that it?
      e
    else e
    // first remove all roots where if they occur 

(* Not entirely sure about this one, maybe it is overkill
  let rec firstSimplify = function
    | FVar x          -> FVar x
    | FNum c          -> FNum c
    | FRoot(e1,n)     -> match e1 with
                         | FNum c -> FNum (c**(1.0/(float n)))
                         | _      -> FRoot (firstSimplify e1, n)
    | FDiv(e1,e2)     -> FDiv(firstSimplify e1, firstSimplify e2)
    | FAdd(e1,e2)     -> FAdd(firstSimplify e1,firstSimplify e2)
    | FMult(e1,e2)    -> FMult(firstSimplify e1,firstSimplify e2)
    | FExponent(e,n)  -> FExponent(firstSimplify e,n)
*)
  let rewriteExpr e =
    // simplififying the expression into one big division of A/B, or if no division exists, just pass e along
    let ex = if containsDiv e 
             then let simpler = simplifyDiv e
                  match simpler with
                  | FDiv(keep,_)  -> keep
                  | _             -> simpler
             else e
    // handle roots
    if containsRadicals ex then simplifyRadicals ex
    else ex
  // containsRadicals (rewriteExpr (parseStr "1 + 2 + 3 + 4_2 + 5 + 6_1"))
  let simplifyAtomGroup ag : atomGroup =
      let mutable nums = 1.0
      let mutable exps = Map.empty
      for  a in ag do
        match a with
          | ANum n          -> nums <- n * nums
          | AExponent(x,n)  -> 
            match Map.tryFind x exps with
              | Some v  -> exps <- Map.add x (n + v) exps
              | None    -> exps <- Map.add x n exps
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
      | [ANum n]      -> nums <- n + nums // Add atom groups with only constants together.
      | ANum n::cr    -> match Map.tryFind cr vars with
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

  let exprToSimpleExpr (e:expr) :simpleExpr = simplifySimpleExpr (SE (simplify e))

  type poly = P of Map<int,simpleExpr>

  let ppPoly v (P p) =
    let pp (d,ags) =
      let prefix = if d=0 then "" else ppAtom (AExponent(v,d))
      let postfix = if isSimpleExprEmpty ags then "" else "(" + (ppSimpleExpr ags) + ")"
      prefix + postfix
    String.concat "+" (List.map pp (Map.toList p))

  (* Collect atom groups into groups with respect to one variable v *)
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

  let solveSimpleExpr (SE agl) m : float =
    let rec innerAG = function
    | []     -> 1.0
    | a :: r -> match a with
                | ANum f         -> f * innerAG r
                | AExponent(e,x) -> 
                    let v = Map.find e m
                    if x = 1 then v * innerAG r 
                    else v**(float x) * innerAG r
    let rec innerSE = function
      | []      -> 0.0
      | ag :: r -> (innerAG ag) + (innerSE r)
    innerSE agl

 (* Simple tests

  let x = parseStr "x^2 - x^2"
  let x1 = simplify x
  List.map simplifyAtomGroup x1
  
  let x = parseStr "x^2 - x^2"
  let x1 = simplify x
  List.map simplifyAtomGroup x1
  let x = FAdd (FExponent (FVar "x", 2) ,
                         FAdd (FExponent (FVar "y", 2),
                               FAdd (FExponent (FVar "z", 2),
                                     FMult (FNum -1.0, FExponent (FVar "r", 2))))) 
  let (SE z) = exprToSimpleExpr x
  let y = simplify x
  List.map simplifyAtomGroup y
  

  let (SE z) = exprToSimpleExpr x
  List.map simplifyAtomGroup z
  ppPoly "" (exprToPoly x "") 

  ppPoly "" (exprToPoly (parseStr "-x * (y - z)") "")

  ppPoly "" (exprToPoly (parseStr "(x + y) / z") "")
  
  ppPoly "" (exprToPoly (parseStr "10.0 / 5.5") "")

  ppPoly "" (exprToPoly (parseStr "x^3 / x^2") "")

  ppPoly "" (exprToPoly (parseStr "x^3 / x^2") "")

  ppPoly "" (exprToPoly (parseStr "x^2 - x^2") "")
  ppPoly "" (exprToPoly (parseStr "x^2 + y^2 + 1") "")
  ppPoly "" (exprToPoly (parseStr "x^2 + x^2 + 1x^2") "")
  ppPoly "" (exprToPoly (parseStr "x^2 + y^2 + y^2") "")
  ppPoly "" (exprToPoly (parseStr "x*x*y*x*y") "y")
  ppPoly "" (exprToPoly (parseStr "10 x * x * x + 2 x + 1") "")
  ppPoly "" (exprToPoly (parseStr " x * x * x + 2") "")
  ppPoly "" (exprToPoly (parseStr "x^2 + y^2 + z^2 - 1") "")
  ppPoly "" (exprToPoly (parseStr "x^2 + y^2 + z^2 - R") "")
  ppPoly "" (exprToPoly (parseStr "- (r * r)") "")
  ppPoly "" (exprToPoly (parseStr "- r * r") "")
  ppPoly "" (exprToPoly (parseStr "2_3") "")
  ppPoly "" (exprToPoly (parseStr "(3 * 3)_2") "")
  ppPoly "" (exprToPoly (parseStr "- (r * r)_2") "")
  ppPoly "" (exprToPoly (parseStr "- r * r _ 2") "")

   ppPoly "" (exprToPoly (parseStr  "(x^2 + (4.0/9.0)*y^2 + z^2 - 1)^3 - x^2 * z^3 - (9.0/80.0)*y^2*z^3") "")
  *)