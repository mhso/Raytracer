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
    let dx = partialDerivative "x" e
    let dy = partialDerivative "y" e
    let dz = partialDerivative "z" e
    let hitFunction (r:Ray) =
      let m = getVarMap r
      let a = solveSE m 0.0 aSimple
      let b = solveSE m 0.0 bSimple
      let t = (-b) / a
      if t < 0.0 then None
      else 
        let c = new Colour(1.,1.,1.)
        Some (t, normalVector (r.PointAtTime t) dx dy dz)
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
    let dx = partialDerivative "x" e
    let dy = partialDerivative "y" e
    let dz = partialDerivative "z" e
    let hitFunction (r:Ray) =
      let m = getVarMap r
      let a = solveSE m 0.0 aSimple
      let b = solveSE m 0.0 bSimple
      let c = solveSE m 0.0 cSimple
      if discriminant a b c < 0.0 then None
      else
        let ts = getDistances a b c |> List.filter (fun x -> x >= 0.0)
        if List.isEmpty ts then None
        else
          let t' = List.min ts
          let hp = r.PointAtTime t'
          Some (t', normalVector hp dx dy dz)
    hitFunction

  // based on the pseudo code given here: https://en.wikipedia.org/wiki/Newton%27s_method#Pseudocode
  // but adapted to a functional, immutable, approach
  let newtonRaphson f f' initial =
    let tolerance = 0.000001 // 7 digit accuracy is desired
    let epsilon = 0.0000000000001 // Don't want to divide by a number smaller than this
    let rec inner g iter =
      if iter < 0 then None
      else
        let y  = solveUnipoly f g
        let y' = solveUnipoly f' g
        if abs y' < epsilon then None
        else
          let g' = g - (y / y')
          if abs (g' - g) <= (tolerance * abs g')
            then Some g'
          else
            inner g' (iter - 1)
    inner initial 20

  let getHigherDegreeHF p e =
    // pre-processing parts of the normalVector
    let dx = partialDerivative "x" e
    let dy = partialDerivative "y" e
    let dz = partialDerivative "z" e
    let hitFunction r =
      let m = getVarMap r
      let up = polyToUnipoly p m
      let up' = unipolyDerivative up
      let ss = sturmSeq up up'
      let g = makeGuess ss
      match g with
      | None    -> None
      | Some v  -> 
          let x = newtonRaphson up up' v
          match x with
          | None    -> None
          | Some t  -> 
              let hp = r.PointAtTime t
              Some (t, normalVector hp dx dy dz)
    hitFunction

  let mkImplicit (s:string) : baseShape =
    let exp = parseStr s // parsing the equation string to expression
    let (P m) = (substWithRayVars >> exprToPoly) exp "t" // converting the expression to a polynomial
    let hitfunction =
      match getOrder m with
      | 1 -> getFirstDegreeHF (P m) exp
      | 2 -> getSecondDegreeHF (P m) exp
      | _ -> getHigherDegreeHF (P m) exp
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