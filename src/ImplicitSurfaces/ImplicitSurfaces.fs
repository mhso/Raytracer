namespace Tracer.ImplicitSurfaces

module Main =

  open Tracer.ImplicitSurfaces.ExprParse
  open Tracer.ImplicitSurfaces.ExprToPoly
  open Tracer.ImplicitSurfaces.PolyToUnipoly
  open Tracer.Basics

  type hf = Ray -> (float * Vector) option
  type hitPoint = Tracer.Basics.HitPoint
  type baseShape = Tracer.BaseShape.BaseShape
  type shape = Tracer.Basics.Shape

  type expr = ExprParse.expr
  type poly = ExprToPoly.poly
  type unipoly = PolyToUnipoly.unipoly
  type Ray = Tracer.Basics.Ray

  let substWithRayVars (e:expr) = 
      let ex = FAdd(FVar "ox", FMult(FVar "t",FVar "dx"))
      let ey = FAdd(FVar "oy", FMult(FVar "t",FVar "dy"))
      let ez = FAdd(FVar "oz", FMult(FVar "t",FVar "dz"))
      List.fold subst e [("x",ex);("y",ey);("z",ez)]

  let rec containsVar var = function
    | FVar x        -> x = var
    | FNum _        -> false
    | FAdd(e1,e2)   -> containsVar var e1 || containsVar var e2
    | FMult(e1,e2)  -> containsVar var e1 || containsVar var e2
    | FDiv(e1,e2)   -> containsVar var e1 || containsVar var e2 
    | FExponent(e,_)-> containsVar var e
    | FRoot(e,_)    -> containsVar var e

  // returns a partial derivative with respect to var
  let rec partialDerivative var e =
    let rec inner = function
    // the follow rewrites are based on the chain rule
      | FNum _          -> FNum 0.0 // case 1
      | FVar x          -> if x <> var then FNum 0.0 // case 1
                           else FNum 1.0 // case 2
      | FAdd(e1, e2)    -> FAdd (inner e1, inner e2) // case 3
      | FMult(e1, e2)   -> FAdd (FMult (inner e1, e2), FMult (inner e2, e1)) // case 4
      | FDiv(e1, e2)    -> FDiv (FAdd (FMult (e2, inner e1), FMult (FNum -1.0, FMult (e1, inner e2))), FExponent(e2,2)) // case 5
      | FExponent(e1, n)-> FMult(inner e1, FMult (FNum (float n), FExponent(e1, n-1))) // case 6
      | FRoot(e1, n)    -> FDiv(inner e1, FMult (FNum (float n), FExponent(FRoot(e1, n), n-1))) // case 7
    (inner >> reduceExpr) e

  let getPointMap (p:Point) =
    Map.empty
      .Add("x",p.X)
      .Add("y",p.Y)
      .Add("z",p.Z)

  // thou shall not be simplified!
  // returns a vector, based on the initital shape equation, and partially derived with respect to x, y, and z from the hitpoint
  let normalVector p dx dy dz  =
    let m = getPointMap p
    let x = solveExpr m dx
    let y = solveExpr m dy
    let z = solveExpr m dz
    Vector(x, y, z).Normalise

  let discriminant (a:float) (b:float) (c:float) =
    b**2.0 - 4.0 * a * c

  let getDistances a b c = 
    let sres = sqrt((b**2.0) - 4.0 * a * c)
    let ares = 2.0 * a
    let res f = (f (-b) (sres)) / ares
    [res (+); res (-)]

  let getVarMap (r:Ray) = 
    Map.empty 
      .Add("ox", r.GetOrigin.X)
      .Add("oy", r.GetOrigin.Y)
      .Add("oz", r.GetOrigin.Z)
      .Add("dx", r.GetDirection.X)
      .Add("dy", r.GetDirection.Y)
      .Add("dz", r.GetDirection.Z)

  let getFirstDegreeHF (P m) e : hf =
    let aSimple = match Map.tryFind 1 m with
                  | Some v -> v
                  | None   -> SE []
    let bSimple = match Map.tryFind 0 m with
                  | Some v -> v
                  | None   -> SE []
    let pdx = partialDerivative "x" e
    let pdy = partialDerivative "y" e
    let pdz = partialDerivative "z" e
    let hitFunction (r:Ray) =
      let ox,oy,oz = r.GetOrigin.GetCoord
      let dx,dy,dz = r.GetDirection.GetCoord
      let a = solveSE aSimple ox oy oz dx dy dz
      let b = solveSE bSimple ox oy oz dx dy dz
      let t = (-b) / a
      if t < 0.0 then None
      else 
        let c = new Colour(1.,1.,1.)
        Some (t, normalVector (r.PointAtTime t) pdx pdy pdz)
    hitFunction

  let getSecondDegreeHF (P m) e :hf = 
    let aSimple = match Map.tryFind 2 m with
                  | Some v -> v
                  | None   -> SE []
    let bSimple = match Map.tryFind 1 m with
                  | Some v -> v
                  | None   -> SE []
    let cSimple = match Map.tryFind 0 m with
                  | Some v -> v
                  | None   -> SE []
    let pdx = partialDerivative "x" e
    let pdy = partialDerivative "y" e
    let pdz = partialDerivative "z" e

    let hitFunction (r:Ray) =
      let ox,oy,oz = r.GetOrigin.GetCoord
      let dx,dy,dz = r.GetDirection.GetCoord
      let a = solveSE aSimple ox oy oz dx dy dz
      let b = solveSE bSimple ox oy oz dx dy dz
      let c = solveSE cSimple ox oy oz dx dy dz
      if discriminant a b c < 0.0 then None
      else
        let ts = getDistances a b c |> List.filter (fun x -> x >= 0.0)
        if List.isEmpty ts then None
        else
          let t' = List.min ts
          let hp = r.PointAtTime t'
          Some (t', normalVector hp pdx pdy pdz)
    hitFunction

  let nrtolerance = 10.**(-7.)
  let nrepsilon = 10.**(-14.)
  
  // based on the pseudo code given here: https://en.wikipedia.org/wiki/Newton%27s_method#Pseudocode
  // but adapted to a functional, immutable, approach
  let newtonRaphson f f' initial =
    let rec inner g iter =
      if iter < 0 then None
      else
        let y  = solveUnipoly f g
        let y' = solveUnipoly f' g
        if abs y' < nrepsilon then None
        else
          let g' = g - (y / y')
          if abs (g' - g) <= (nrtolerance * abs g')
            then Some g'
          else
            inner g' (iter - 1)
    inner initial 15

  let getHigherDegreeHF p e =
    // pre-processing parts of the normalVector
    let pdx = partialDerivative "x" e
    let pdy = partialDerivative "y" e
    let pdz = partialDerivative "z" e
    let hitFunction (r:Ray) =
      let ox,oy,oz = r.GetOrigin.GetCoord
      let dx,dy,dz = r.GetDirection.GetCoord
      let up = polyToUnipoly p ox oy oz dx dy dz
      let up' = unipolyDerivative up
      let ss = sturmSeq up up'
      let rec findx l h max itcount =
        if itcount > 4 then None // don't wanna end in an endless loop
        else 
          match getInterval ss l h max with
          | None              -> None
          | Some (lo,hi,mid)  ->
              match newtonRaphson up up' mid with
              | None    -> None
              | Some t  ->
                  if t < lo then 
                    printfn "t < lo"
                    findx mid hi 5 (itcount + 1)
                  else 
                    if t > hi then 
                      printfn "t > hi"
                      findx lo mid 5 (itcount + 1)
                    else
                      let hp = r.PointAtTime t
                      Some (t, normalVector hp pdx pdy pdz)
      findx 0.0 100.0 15 0
    hitFunction

  let mkImplicit (s:string) : baseShape =
    let exp = parseStr s // parsing the equation string to expression
    let (P m) = (substWithRayVars >> exprToPoly) exp "t" // converting the expression to a polynomial
    let hitfunction =
      match getOrder m with
      | 1 -> getFirstDegreeHF (P m) exp
      | 2 -> getSecondDegreeHF (P m) exp
      | _ -> getHigherDegreeHF (polyAsList (P m)) exp
    let bsh = 
        { new baseShape() with
            member this.toShape tex =
              let mat = (Textures.getFunc tex) 1. 1.
              { new shape() with
                  member this.hitFunction r = 
                    match hitfunction r with
                    | None        -> hitPoint (r)
                    | Some (t,v)  -> hitPoint (r, t, v, mat, this)
                  member this.isInside p = (solveExpr << getPointMap) p exp < 0.0
                  member this.getBoundingBox () = failwith "getBoundingBox not implemented for implicit surfaces"
              }
          }
    bsh

(*
  [<EntryPoint>]
  let main argv =
    printfn "we are running this shit!"
    0 // return a beautiful integer exit code
*)