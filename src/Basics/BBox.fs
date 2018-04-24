﻿namespace Tracer.Basics

type BBox(lowPoint:Point, highPoint:Point) =
    member this.lowPoint = lowPoint
    member this.highPoint = highPoint
    member this.intersect (r:Ray) =
        let tx = if r.GetDirection.X >= 0.0 then (lowPoint.X - r.GetOrigin.X)/r.GetDirection.X else (highPoint.X - r.GetOrigin.X)/r.GetDirection.X
        let tx' = if r.GetDirection.X >= 0.0 then (highPoint.X - r.GetOrigin.X)/r.GetDirection.X else (lowPoint.X - r.GetOrigin.X)/r.GetDirection.X
        let ty = if r.GetDirection.Y >= 0.0 then (lowPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y else (highPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let ty' = if r.GetDirection.Y >= 0.0 then (highPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y else (lowPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let tz = if r.GetDirection.Z >= 0.0 then (lowPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z else (highPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z
        let tz' = if r.GetDirection.Z >= 0.0 then (highPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z else (lowPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z
        

        let t = max tx (max ty tz)

        let t' = min tx' (min ty' tz')

        if t < t' && t' > 0.0 then Some(t, t') else None


