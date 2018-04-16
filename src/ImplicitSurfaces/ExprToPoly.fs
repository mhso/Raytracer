namespace Tracer.ImplicitSurfaces

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
    | FVar s          -> if s = x then ex else FVar s
    | FAdd(a,b)       -> FAdd(subst a (x,ex), subst b (x, ex))
    | FMult(a,b)      -> FMult(subst a (x,ex), subst b (x,ex))
    | FExponent(a,i)  -> FExponent(subst a (x,ex), i)
    | FDiv(a,b)       -> FDiv(subst a (x,ex), subst b (x,ex))
    | FRoot(a,i)      -> FRoot(subst a (x,ex), i)

  type atom = ANum of float | AExponent of string * int
  type atomGroup = atom list  
  type simpleExpr = SE of atomGroup list
  let isSimpleExprEmpty (SE ags) = List.isEmpty ags || ags = [[]]

  let ppAtom = function
    | ANum c          -> string(c)
    | AExponent(s,1)  -> s
    | AExponent(s,n)  -> s+"^"+(string(n))
  let ppAtomGroup ag = String.concat "*" (List.map ppAtom ag)
  let ppSimpleExpr (SE ags) = String.concat "+" (List.map ppAtomGroup ags)

  // xss is multiplied on all the ys's that are found. At least that's what *I think is happening.
  let rec combine (xss:atomGroup list) = function
    | [] -> []
    | ys::yss -> List.map ((@) ys) xss @ combine xss yss


  // Invoking the spirit of Muhammad ibn Musa al-Khwarizmi
  let rec simplify = function
    | FNum c          -> [[ANum c]]
    | FVar s          -> [[AExponent(s,1)]]
    | FAdd(e1,e2)     -> simplify e1 @ simplify e2
    | FMult(e1,e2)    -> combine (simplify e1) (simplify e2)
    | FDiv(e1,e2)     -> combine (simplify e1) (simplify (FExponent(e2, -1))) // e1 / e2 is the same e1 * e2^-1 (because e2^-1 = 1 / e2)
    | FExponent(_,0)  -> simplify (FNum 1.0)
    | FExponent(e1,1) -> simplify e1
    | FExponent(e1,n) -> if n < 0 then 
                            match e1 with
                            | FNum c            -> combine [[ANum (1./c)]] (simplify (FExponent(e1, n + 1)))
                            | FVar s1           -> combine [[AExponent(s1, -1)]] (simplify (FExponent(e1, n + 1)))
                            | _       -> failwith "simplify: unmatched expr" // TODO: I need to figure out what to do when we encounter other stuff
                         else combine (simplify e1) (simplify (FExponent(e1, n-1)))
    | FRoot _         -> failwith "simplify: FRoot not implemented"

  let rec containsDiv = function
    | FVar _          -> false
    | FNum _          -> false
    | FDiv _          -> true
    | FAdd(e1,e2)     -> containsDiv e1 || containsDiv e2
    | FMult(e1,e2)    -> containsDiv e1 || containsDiv e2
    | FExponent(e,_)  -> containsDiv e
    | FRoot(e,_)      -> containsDiv e

  let rec rewriteExpr e =
    let rec inner ex =
      match ex with
      | FExponent(_,0)  -> FNum 1.0       // case 1: e^0 = 1
      | FExponent(e,1)  -> inner e        // case 2: e^1 = e
      | FExponent(e,-1) -> FExponent (inner e, -1) // case 3: e^n+1 = e * e^n 
      | FExponent(e,n)  -> if n > 1 then
                              FMult (inner e, inner (FExponent(e, n - 1))) // not completely sure about these two
                           else FMult(FExponent(inner e, -1), inner(FExponent(inner e, n + 1)))
      // case 4: e1 * (e2 + e3) = e1 * e2 + e1 * e3 
      // actually I think case 4 is covered in simplify...
      // case 5: the negative variant is already covered by e1 - e2 = e1 + (-1 * e2) <- from ExprParse.fs
      | FMult(e1, FAdd(e2,e3)) -> FAdd(FMult(inner e1, inner e2), FMult(inner e1, inner e3))
      // case 6: e1 * (e2 / e3) = (e1 * e2) / e3
      | FMult(e1, FDiv(e2, e3)) -> FDiv(FMult(inner e1, inner e2), inner e3)
      | FMult(FDiv(e2, e3), e1) -> FDiv(FMult(inner e1, inner e2), inner e3)
      // case 10: (e1 / e2) / (e3 / e4)
      | FDiv(FDiv(e1, e2), FDiv(e3, e4)) -> FDiv(FMult(inner e1, inner e4), FMult(inner e2, inner e3))
      // case 7: (e1 / e2) / e3 = e1 / (e2 * e3)
      | FDiv(FDiv(e1, e2), e3) -> FDiv(inner e1, FMult (inner e2, inner e3))
      // case 8: e1 / (e2 / e3) = (e1 * e3) / e2
      | FDiv(e1, FDiv(e2, e3)) -> FDiv(FMult(inner e1, inner e3), inner e2)
      // case 9: e1 + (e2 / e3) = (e1 * e3 + e2) / e3
      | FAdd(e1, FDiv(e2, e3)) -> FDiv(FAdd(FMult(inner e1, inner e3), inner e2), inner e3)
      // case 11: e_n * ... * e_n (n times) = e
      // not implemented yet! I think I need a class by its own for that...
      // rest of the patterns should either delve further down the tree, or in the case of FNum and FVar, return ex
      | FAdd(e1,e2)   -> FAdd(inner e1, inner e2)
      | FMult(e1, e2) -> FMult(inner e1, inner e2)
      | FRoot(e1, n)  -> FRoot(inner e1, n)
      | FDiv(e1, e2)  -> FDiv(inner e1, inner e2)
      | _             -> ex
    let rewritten = inner e
    if containsDiv rewritten then
      match rewritten with
      | FDiv(keep, _)   -> keep
      | _               -> rewriteExpr rewritten
    else rewritten
  




  (*
    case 1:
    let x = rewriteExpr (simplifyDiv (parseStr "e^0"))
    = 1
    working

    case 2:
    let x = rewriteExpr (simplifyDiv (parseStr "e^1"))
    = e
    working

    case 3:
    let x = rewriteExpr (simplifyDiv (parseStr "e^4"))
    = e * e * e * e
    working

    case 4:
    let x = rewriteExpr (simplifyDiv (parseStr "e1*(e2 + e3)"))
    = e1 * e2 + e1 * e3
    working

    case 5:
    let x = rewriteExpr (simplifyDiv (parseStr "e1 * (e2 - e3)"))
    = e1 * e2 - e1 * e3
    working

    case 6:
    let x = rewriteExpr (simplifyDiv (parseStr "e1 * (e2 / e3)"))
    = (e1 * e2) / e3
    WORKING (not really working in simplify)
    
    case 7:
    let x = rewriteExpr (simplifyDiv (parseStr "(e1 / e2) / e3"))
    =  e1 / (e2 * e3)
    SEMI working (not ideally working in simplify, or what?)

    case 8:
    let x = rewriteExpr (simplifyDiv (parseStr "e1 / (e2 / e3)"))
    = (e1 * e3) / e2
    NOT WORKING!

    case 9
    let x = rewriteExpr (simplifyDiv (parseStr "e1 + (e2 / e3)"))
    = (e1 * e3 + e2) / e3
    SEMI WORKING

    case 10:
    let x = rewriteExpr (simplifyDiv (parseStr "(e1 / e2) / (e3 / e4)"))
    = (e1 * e4) / (e2 * e3)
    NOT WORKING

    case 11:
    let x = rewriteExpr (simplifyDiv (parseStr "e_3 * e_3 * e_3"))
    = e
    BIG CRASH!!!!! BOOOOOM

    let x1 = simplify x
    *)

  let rec simplifyDiv = function
    | FAdd(e1,e2)     -> FAdd(simplifyDiv e1, simplifyDiv e2)
    | FMult(e1,e2)    -> FMult(simplifyDiv e1, simplifyDiv e2)
    | FNum c          -> FNum c
    | FVar x          -> FVar x
    | FExponent(e1,n) -> FExponent(e1,n)
    | FRoot(e1,n)     -> FRoot (e1,n)
    | FDiv(e1,e2)     -> match e2 with
                         | FVar x         -> FMult(e1, FExponent(e2, -1))
                         | FNum c         -> FMult(e1, FExponent(e2, -1))
                         | FExponent(_,0) -> FDiv(e1, FNum 1.0)
                         | FExponent(e,n) -> FMult(e1, (FExponent(e, -n)))
                         | _              -> FDiv(e1, e2)
   

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
      | (ANum n)::cr  -> match Map.tryFind cr vars with
                          | Some v  -> vars <- Map.add cr (v + n) vars
                          | None    -> vars <- Map.add cr n vars
      | []            -> nums <- 0.0 
      | _             -> failwith "simplifySimpleExpr: unmatched clause" // should never get here                   
    ) ags'
    // Last task is to group similar atomGroups into one group.
    let varslist = [for KeyValue(k,v) in vars -> if v = 1.0 then k
                                                 //else if v = 0.0 then [ANum 0.0]
                                                 else (ANum v)::k]
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