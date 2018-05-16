namespace Tracer

module ImplicitSurfaces =

  open Tracer.ExprParse
  open Tracer.ExprToPoly
  open Tracer.PolyToUnipoly
  open Tracer.Basics
  open Tracer.Basics.Textures

  type hf = Ray -> (float * Vector) option
  type hitPoint = Tracer.Basics.HitPoint
  type baseShape = Tracer.BaseShape.BaseShape
  type shape = Tracer.Basics.Shape

  type expr = ExprParse.expr
  type poly = ExprToPoly.poly
  type unipoly = PolyToUnipoly.unipoly
  type Ray = Tracer.Basics.Ray
  type simpleIntExpr = PolyToUnipoly.simpleIntExpr
  type simpleExpr = ExprToPoly.simpleExpr

  let substWithRayVars (e:expr) = 
      let ex = FAdd(FVar "ox", FMult(FVar "t",FVar "dx"))
      let ey = FAdd(FVar "oy", FMult(FVar "t",FVar "dy"))
      let ez = FAdd(FVar "oz", FMult(FVar "t",FVar "dz"))
      List.fold subst e [("x",ex);("y",ey);("z",ez)]

  // returns a partial derivative with respect to var
  let rec partialDerivative var e =
    let rec inner = function
    // the follow rewrites are based on the chain rule
      | FNum _          -> FNum 0.0 // case 1
      | FVar x          -> if x <> var then FNum 0.0 // case 1
                           else FNum 1.0 // case 2
      | FExponent(e1, n)-> FMult(inner e1, FMult (FNum (float n), FExponent(e1, n-1))) // case 6
      | FAdd(e1, e2)    -> FAdd (inner e1, inner e2) // case 3
      | FMult(e1, e2)   -> FAdd (FMult (inner e1, e2), FMult (inner e2, e1)) // case 4
      | FDiv(e1, e2)    -> FDiv (FAdd (FMult (e2, inner e1), FMult (FNum -1.0, FMult (e1, inner e2))), FExponent(e2,2)) // case 5
      | FRoot(e1, n)    -> FDiv(inner e1, FMult (FNum (float n), FExponent(FRoot(e1, n), n-1))) // case 7
    (inner >> reduceExpr) e

  // thou shall not be simplified!
  // returns a vector, based on the initital shape equation, and partially derived with respect to x, y, and z from the hitpoint
  let normalVector p dx dy dz  =
    let x = solveExpr p dx
    let y = solveExpr p dy
    let z = solveExpr p dz
    Vector(x, y, z).Normalise

  let discriminant (a:float) (b:float) (c:float) =
    (pown b 2) - 4.0 * a * c
  
  let getDistances a b d = 
    let sres = sqrt d
    let res f = (f (-b) sres) / (2.0 * a)
    [res (+); res (-)]

  let getValArray (r:Ray) = 
    let arr = Array.zeroCreate 6
    arr.[0] <- r.GetOrigin.X // ox
    arr.[1] <- r.GetOrigin.Y // oy
    arr.[2] <- r.GetOrigin.Z // oz
    arr.[3] <- r.GetDirection.X // dz
    arr.[4] <- r.GetDirection.Y // dy
    arr.[5] <- r.GetDirection.Z // dz
    arr

  let nrtolerance = 10.**(-5.)
  let nrepsilon = 10.**(-10.)
  
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
    inner initial 20

  let sepolyToSIEpoly p = List.foldBack (fun ((n:int),c) acc -> (n,seToSIE c)::acc) p []

  let getFirstDegreeHF (plst:(int*simpleIntExpr) list) pdx pdy pdz : hf =
    let mutable plst = plst
    let aSIE = if not plst.IsEmpty && fst plst.[0] = 1 then
                  let res = snd plst.[0]
                  plst <- plst.Tail
                  res
               else SIE [[]]
    let bSIE = if not plst.IsEmpty && fst plst.[0] = 0 then
                  snd plst.[0]
               else SIE [[]]
    let hitFunction (r:Ray) =
      let valArray = getValArray r
      let a = solveSIE aSIE valArray
      let b = solveSIE bSIE valArray
      let t = (-b) / a
      if t < 0.0 then None
      else 
        let c = new Colour(1.,1.,1.)
        Some (t, normalVector (r.PointAtTime t) pdx pdy pdz)
    hitFunction

  let getSecondDegreeHF (plst:(int*simpleIntExpr) list) pdx pdy pdz :hf =
    let mutable plst = plst
    let aSIE = snd plst.[0] // we know it is degree 2, otherwise we douldn't be here
    plst <- plst.Tail
    let bSIE = if not plst.IsEmpty && fst plst.[0] = 1 then
                  let res = snd plst.[0]
                  plst <- plst.Tail
                  res
               else SIE [[]]
    let cSIE = if not plst.IsEmpty && fst plst.[0] = 0 then
                  snd plst.[0]
               else SIE [[]]
    let hitFunction (r:Ray) =
      let valArray = getValArray r
      let a = solveSIE aSIE valArray
      let b = solveSIE bSIE valArray
      let c = solveSIE cSIE valArray
      let d = discriminant a b c
      if d < 0.0 then None
      else
        let ts = getDistances a b d |> List.filter (fun x -> x >= 0.0)
        if List.isEmpty ts then None
        else
          let t' = List.min ts
          let hp = r.PointAtTime t'
          Some (t', normalVector hp pdx pdy pdz)
    hitFunction

  let getHigherDegreeHF plst pdx pdy pdz =
    let hitFunction (r:Ray) =
      let valArray = getValArray r
      // now that we know the Ray values, we can turn our multivariable polynomial into a univariate one
      let up = toUnipoly plst valArray
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
                  if t < lo then findx mid hi 5 (itcount + 1)
                  else 
                    if t > hi then findx lo mid 5 (itcount + 1)
                    else
                      let hp = r.PointAtTime t
                      Some (t, normalVector hp pdx pdy pdz)
      findx 0.0 100.0 15 0
    hitFunction

  let mkImplicit (s:string) : baseShape =
    let exp = parseStr s // parsing the equation string to expression
    // partial derivates, needed for the normal, returned when a hit occurs
    let pdx = partialDerivative "x" exp
    let pdy = partialDerivative "y" exp
    let pdz = partialDerivative "z" exp
    // converting the expression to a polynomial
    let p = (substWithRayVars >> exprToPoly) exp "t"
    // turning the polynomial into a list, since it's faster to work with, with our need
    let plst = (polyAsList >> sepolyToSIEpoly >> List.rev) p

    let hitfunction =
      match fst (plst.[0]) with
      | 1 -> getFirstDegreeHF plst pdx pdy pdz
      | 2 -> getSecondDegreeHF plst pdx pdy pdz
      | _ -> getHigherDegreeHF plst pdx pdy pdz
    let bsh = 
        { new baseShape() with
            member this.toShape tex =
              let mat = (Textures.getFunc tex) 1. 1.
              { new shape() with
                  member this.hitFunction r = 
                    match hitfunction r with
                    | None        -> hitPoint (r)
                    | Some (t,v)  -> hitPoint (r, t, v, mat, this)
                  member this.isInside p = solveExpr p exp < 0.0
                  member this.getBoundingBox () = failwith "getBoundingBox not implemented for implicit surfaces"
              }
          }
    bsh