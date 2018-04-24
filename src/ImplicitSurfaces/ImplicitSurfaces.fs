namespace Tracer.ImplicitSurfaces

module Main =

  open Tracer.ImplicitSurfaces.ExprParse
  open Tracer.ImplicitSurfaces.ExprToPoly
  open Tracer.Basics

  type Vector = Tracer.Basics.Vector
  type Point = Tracer.Basics.Point

  type Ray(o: Point, d: Vector) = 
    member this.GetOrigin = o
    member this.GetDirection = d.Normalise
    // Returns a point from a given time/length of the ray
    member this.PointAtTime (t:float) = 
        o + (t * d)

  type hf = Ray -> (float * Vector) option

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

  let rec partial var = function // the follow rewrites are based on the chain rule
    | FNum c          -> FNum 0.0 // case 1
    | FVar x          -> if x <> var then FNum 0.0 // case 1
                         else FNum 1.0 // case 2
    | FAdd(e1, e2)    -> FAdd (partial var e1, partial var e2) // case 3
    | FMult(e1, e2)   -> FAdd (FMult (partial var e1, e2), FMult (partial var e2, e1)) // case 4
    | FDiv(e1, e2)    -> FDiv (FAdd (FMult (e2, partial var e1), FMult (FNum -1.0, FMult (e1, partial var e2))), FExponent(e2,2)) // case 5
    | FExponent(e1, n)-> FExponent(FMult (partial var e1, FMult (FNum (float n), e1)), n-1) // case 6
    | FRoot(e1, n)    -> FDiv(partial var e1, FMult (FNum (float n), FExponent(FRoot(e1, n), n-1))) // case 7

  // thou shall not be simplified!
  let derivative (e:expr) px py pz =
    let m = Map.empty
              .Add("x",px)
              .Add("y",py)
              .Add("z",pz)
    let x = solveExpr m (partial "x" e)
    let y = solveExpr m (partial "y" e)
    let z = solveExpr m (partial "z" e)
    Vector(x, y, z)

  let discriminant (a:float) (b:float) (c:float) =
    b**2.0 - 4.0 * a * c

  let getDistances a b c = 
    let sres = sqrt((b**2.0) - 4.0 * a * c)
    let ares = 2.0 * a
    let res f = (f (-b) (sres)) / ares
    [res (+); res (-)]

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

    let hitFunction (r:Ray) =
      let m = Map.empty 
                    .Add("ox", r.GetOrigin.X)
                    .Add("oy", r.GetOrigin.Y)
                    .Add("oz", r.GetOrigin.Z)
                    .Add("dx", r.GetDirection.X)
                    .Add("dy", r.GetDirection.Y)
                    .Add("dz", r.GetDirection.Z)
      let a = solveSimpleExpr m aSimple
      let b = solveSimpleExpr m bSimple
      let c = solveSimpleExpr m cSimple
      if discriminant a b c < 0.0 then None
      else
        let ts = getDistances a b c |> List.filter (fun x -> x >= 0.0)
        if List.isEmpty ts then None
        else
          let t' = List.min ts
          let hp = r.PointAtTime t'
          Some (t', derivative e hp.X hp.Y hp.Z)

    hitFunction

  let mkImplicit (s:string) : hf =
    let exp = parseStr s
    let (P m) = (substWithRayVars >> exprToPoly) exp "t"
    let order = getOrder m
    if order = 2 then
      getSecondDegreeHF (P m) exp
    else
      failwith "poly of higher degree than 2 is not supported yet"

(*
  [<EntryPoint>]
  let main argv =
    printfn "we are running this shit!"
    0 // return a beautiful integer exit code
*)
  (* Test string

  test "x^2 + y^2 + z^2 - 1"
  "(pz^2+py^2+px^2+-1)+t(2*dz*pz+2*dy*py+2*dx*px)+t^2(dz^2+dy^2+dx^2)"

  (parseStr >> substWithRay >> exprToPoly) "x^2 + y^2 + z^2 - 1" "t"

  Test strings from the TracerTestSuite > ImplicitSurfaces.fs

  sphere1:
  makeImplicit "x^2 + y^2 + z^2 - 9.9"
  
  sphere2:
  test "(x^2 + y^2 + z^2)_2 - 3.3"

  torus:
  test "(((x^2 + y^2)_2 - 1.1)^2 + z^2)_2 - 2.2"

  torus2:
  test "x^4 + 2x^2*y^2 + 2x^2*z^2 - 2*(2.2^2 + 1.1^2)*x^2 + y^4 + 2y^2*z^2 + 2*(2.2^2 - 1.1^2)*y^2 + z^4 - 2*(2.2^2 + 1.1^2)*z^2 + (2.2^2 - 1.1^2)^2"

  testShape:
  test "(x - 2)^2(x+2)^2 + (y - 2)^2(y+2)^2 + (z - 2)^2(z+2)^2 + 3(x^2*y^2 + x^2z^2 + y^2z^2) + 6x y z - 10(x^2 + y^2 + z^2) + 22"

  heart:
  test "(x^2 + (4.0/9.0)*y^2 + z^2 - 1)^3 - x^2 * z^3 - (9.0/80.0)*y^2*z^3"
  *)