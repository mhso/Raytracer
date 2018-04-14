namespace Tracer.ImplicitSurfaces

module PolyToHitFuntion =

  open Tracer.ImplicitSurfaces.ExprToPoly

  type Vector = Tracer.Vector
  type Point = Tracer.Point
  type poly = Tracer.ImplicitSurfaces.ExprToPoly.poly
  type simpleExpr = Tracer.ImplicitSurfaces.ExprToPoly.simpleExpr

  let discriminant (a:float) (b:float) (c:float) =
    b**2.0 - 4.0 * a * c

  let getDistances a b c = 
    let sres = sqrt((b**2.0) - 4.0 * a * c)
    let ares = 2.0 * a
    let res f = (f (-b) (sres)) / ares
    (res (+), res (-))

  let getSecondDegreeHF (P m) = 
    let aSimple = Map.find 2 m
    let bSimple = Map.find 1 m
    let cSimple = Map.find 0 m
    
    let hitFunction (p:Point) (v:Vector) =
      let varmap = Map.empty 
                    .Add("ox", p.X)
                    .Add("oy", p.Y)
                    .Add("oz", p.Z)
                    .Add("dx", v.X)
                    .Add("dy", v.Y)
                    .Add("dz", v.Z)
      let a = solveSimpleExpr aSimple varmap
      let b = solveSimpleExpr bSimple varmap
      let c = solveSimpleExpr cSimple varmap
      if discriminant a b c < 0.0 then (-1.0, Vector(0.,0.,0.))
      else
        let (t1, t2) = getDistances a b c
        if t1 < t1 && t1 > 0.0 
          then (t1, Vector(0.,0.,0.))
        else
          if t2 > 0.0
            then (t2, Vector(0.,0.,0.0))
          else
            (-1.0, Vector(0.,0.,0.))

    hitFunction      

