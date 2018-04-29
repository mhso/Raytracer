namespace Tracer.ImplicitSurfaces

module Main =

  open Tracer.ImplicitSurfaces.ExprParse
  open Tracer.ImplicitSurfaces.ExprToPoly
  open Tracer.Basics

  type hf = Ray -> (float * Vector * MatteMaterial) option
  type shape =
    abstract hf : hf
  type baseShape =
    abstract mkShape : Material -> shape
  type expr = ExprParse.expr
  type poly = ExprToPoly.poly
  type Ray = Tracer.Basics.Ray

  let substWithRayVars (e:expr) = 
      let ex = FAdd(FVar "ox", FMult(FVar "t",FVar "dx"))
      let ey = FAdd(FVar "oy", FMult(FVar "t",FVar "dy"))
      let ez = FAdd(FVar "oz", FMult(FVar "t",FVar "dz"))
      List.fold subst e [("x",ex);("y",ey);("z",ez)]

  let getOrder m = Map.toList m |> List.fold (fun m (n,_) -> max m n) 0

  let rec containsVar var = function
    | FVar x        -> x = var
    | FNum _        -> false
    | FAdd(e1,e2)   -> containsVar var e1 || containsVar var e2
    | FMult(e1,e2)  -> containsVar var e1 || containsVar var e2
    | FDiv(e1,e2)   -> containsVar var e1 || containsVar var e2 
    | FExponent(e,_)-> containsVar var e
    | FRoot(e,_)    -> containsVar var e

  // returns a partial derivative with respect to var
  let rec partial var e =
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

  // thou shall not be simplified!
  // returns a derivative vector, based on the initital shape equation, and with respect to x, y, and z from the hitpoint
  let derivative (p:Point) dx dy dz  =
    let m = Map.empty
              .Add("x",p.X)
              .Add("y",p.Y)
              .Add("z",p.Z)
    let x = solveExpr m dx
    let y = solveExpr m dy
    let z = solveExpr m dz
    Vector(x, y, z)

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
    let dx = partial "x" e
    let dy = partial "y" e
    let dz = partial "z" e
    let hitFunction (r:Ray) =
      let m = getVarMap r
      let a = solveSE m aSimple
      let b = solveSE m bSimple
      let t = (-b) / a
      if t < 0.0 then None
      else 
        let c = new Colour(1.,1.,1.)
        Some (t, derivative (r.PointAtTime t) dx dy dz, MatteMaterial(new Colour(1.,1.,1.)))
    hitFunction

  let getSecondDegreeHF (P m) e = 
    let aSimple = match Map.tryFind 2 m with
                  | Some v -> v
                  | None   -> SE []
    let bSimple = match Map.tryFind 1 m with
                  | Some v -> v
                  | None   -> SE []
    let cSimple = match Map.tryFind 0 m with
                  | Some v -> v
                  | None   -> SE []
    let dx = partial "x" e
    let dy = partial "y" e
    let dz = partial "z" e
    let hitFunction (r:Ray) =
      let m = getVarMap r
      let a = solveSE m aSimple
      let b = solveSE m bSimple
      let c = solveSE m cSimple
      if discriminant a b c < 0.0 then None
      else
        let ts = getDistances a b c |> List.filter (fun x -> x >= 0.0)
        if List.isEmpty ts then None
        else
          let t' = List.min ts
          let hp = r.PointAtTime t'
          Some (t', derivative hp dx dy dz, MatteMaterial(Colour.Black))
    hitFunction


  // based on the pseudo code given here: https://en.wikipedia.org/wiki/Newton%27s_method#Pseudocode
  let newtonRaphson f f' g =
    let tolerance = 10.**(-7.) // 7 digit accuracy is desired
    let epsilon = 10.**(-14.) // Don't want to divide by a number smaller than this
    let maxIterations = 20 // Don't allow the iterations to continue indefinitely
    let mutable foundSolution = false // Have not yet converged to a solution
    let mutable x0 = g // the initial value/guess

    for i in [1 .. maxIterations] do
      let y = solveReducedPolyList x0 f
      let y' = solveReducedPolyList x0 f'
      if abs y' < epsilon then () // break
      else
        let x1 = x0 - (y / y')
        if abs (x1 - x0) <= (tolerance * abs x1) then
          foundSolution <- true
          // break
        else foundSolution <- foundSolution
        x0 <- x1

    if foundSolution then Some x0
    else None

  let newtonRaph p r g =
    newtonRaphson
      (reducePolyConstants p (getVarMap r) |> Map.toList)
      ((polyDerivative >> reducePolyConstants) p (getVarMap r) |> Map.toList)
      g

  let mkImplicit (s:string) : shape =
    let exp = parseStr s // parsing the equation string to expression
    let (P m) = (substWithRayVars >> exprToPoly) exp "t" // converting the expression to a polynomial
    let hitfunction =
      match getOrder m with
      | 1     -> getFirstDegreeHF (P m) exp
      | 2     -> getSecondDegreeHF (P m) exp
      | _     -> failwith "poly of higher degree than 2 is not supported yet"
    let sh = { new shape with
                member this.hf = hitfunction
                }
    sh

                

(*
  [<EntryPoint>]
  let main argv =
    printfn "we are running this shit!"
    0 // return a beautiful integer exit code
*)