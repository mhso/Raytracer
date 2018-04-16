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
                            | FExponent(_, 0)   -> [[ANum 1.0]]
                            | FExponent(e, n)   -> simplify (FExponent(e, -n))
                            | _       -> failwith "simplify: unmatched expr" // TODO: I need to figure out what to do when we encounter other stuff
                         else combine (simplify e1) (simplify (FExponent(e1, n-1)))
    | FRoot _         -> failwith "simplify: FRoot not implemented"
    (*

      let x = parseStr "x^2 / x^2"
      let x = parseStr "x^2 - x^2"
      let x = parseStr "x^3 / x^2"
      let x = parseStr "x / x^10"
      let x = parseStr "x / (10 + x)"
      let x1 = simplify x
      List.map simplifyAtomGroup x1
      let (SE z) = exprToSimpleExpr x
      List.map simplifyAtomGroup z
      ppPoly "" (exprToPoly x "")   

      ppPoly "" (exprToPoly (parseStr "x^2") "")
      
      ppPoly "" (exprToPoly (parseStr "x + x +y") "")

      ppPoly "" (exprToPoly (parseStr "x^2 * x * x^4") "")

      ppPoly "" (exprToPoly (parseStr "x^2 / 2") "")

      ppPoly "" (exprToPoly (parseStr "y /x / 2") "")

      ppPoly "" (exprToPoly (parseStr "x / x + x^2+ x^2 / 2 + x^2") "")      

    *)

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