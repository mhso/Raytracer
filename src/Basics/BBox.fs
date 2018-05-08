﻿namespace Tracer.Basics

type BBox(lowPoint:Point, highPoint:Point) =
    member this.lowPoint = lowPoint
    member this.highPoint = highPoint
    override this.ToString() =
            "BBox(Max: "+lowPoint.ToString()+", Min: "+highPoint.ToString()+")"
    override this.GetHashCode() =
        hash (lowPoint, highPoint)
    override this.Equals(x) = 
        match x with
        | :? BBox as box -> this.highPoint = box.highPoint && 
                                    this.lowPoint = box.lowPoint
        | _ -> false
    member this.intersect (r:Ray) =
        let boolX = r.GetDirection.X >= 0.0
        let boolY = r.GetDirection.Y >= 0.0
        let boolZ = r.GetDirection.Z >= 0.0
        
        let tx = if boolX then (lowPoint.X - r.GetOrigin.X)/r.GetDirection.X else (highPoint.X - r.GetOrigin.X)/r.GetDirection.X
        let tx' = if boolX then (highPoint.X - r.GetOrigin.X)/r.GetDirection.X else (lowPoint.X - r.GetOrigin.X)/r.GetDirection.X
        let ty = if boolY then (lowPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y else (highPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let ty' = if boolY then (highPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y else (lowPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let tz = if boolZ then (lowPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z else (highPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z
        let tz' = if boolZ then (highPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z else (lowPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z
        

        let t = max tx (max ty tz)

        let t' = min tx' (min ty' tz')

        if t < t' && t' > 0.0 then Some(t, t') else None


