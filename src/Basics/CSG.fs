namespace Tracer.Basics
open System



///////////////////////////////////
////CONSTRUCTIVE SOLID GEOMETRY////
///////////////////////////////////

type CSGOperator = Union | Intersection | Subtraction | Grouping

type CSG(s1:Shape, s2:Shape, op:CSGOperator) =
    inherit Shape()
    member this.s1 = s1
    member this.s2 = s2
    member this.op = op
    member this.epsilon = 0.000001
    member this.bBox = match op with
                       |Union|Grouping -> //merges the two BBoxes, by combining the highest high coords, and the lowest low coords, to form a new bounding box
                            let bBox1 = s1.getBoundingBox ()
                            let bBox2 = s2.getBoundingBox ()
                            let newLow = Point((min bBox1.lowPoint.X bBox2.lowPoint.X), (min bBox1.lowPoint.Y bBox2.lowPoint.Y), (min bBox1.lowPoint.Z bBox2.lowPoint.Z))
                            let newHigh = Point((max bBox1.highPoint.X bBox2.highPoint.X), (max bBox1.highPoint.Y bBox2.highPoint.Y), (max bBox1.highPoint.Z bBox2.highPoint.Z))
                            BBox(newLow, newHigh)
                       |Intersection -> //chooses the highest of the low point coords, and the lowest of the highpoint coords, to approximate the intersection
                            let bBox1 = s1.getBoundingBox ()
                            let bBox2 = s2.getBoundingBox ()
                            let newLow = Point((max bBox1.lowPoint.X bBox2.lowPoint.X), (max bBox1.lowPoint.Y bBox2.lowPoint.Y), (max bBox1.lowPoint.Z bBox2.lowPoint.Z))
                            let newHigh = Point((min bBox1.highPoint.X bBox2.highPoint.X), (min bBox1.highPoint.Y bBox2.highPoint.Y), (min bBox1.highPoint.Z bBox2.highPoint.Z))
                            BBox(newLow, newHigh)
                       |Subtraction -> s1.getBoundingBox () //just returns the bounding box for s1

    override this.isInside (p:Point) = match op with
                                        |Union|Grouping -> (s1.isInside p || s2.isInside p)
                                        |Intersection -> (s1.isInside p && s2.isInside p)
                                        |Subtraction -> (s1.isInside p && (not (s2.isInside p)))

    override this.getBoundingBox () = this.bBox
    

    member this.checkIntersectForBoundingBox (b:bool) (r:Ray) = 
        match b with
        |true -> (this.bBox.intersect r).IsSome
        |false -> true

                                        

    ////UNION////
    member this.unionHitFunctionInside (originalRay:Ray) (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r
        let s1Time = match s1Hit.DidHit with
                     |true -> s1Hit.Time 
                     |false -> infinity
        let s2Time = match s2Hit.DidHit with
                     |true -> s2Hit.Time 
                     |false -> infinity

        //i continue no matter what 

        //compare the two times, and continue to work with the closest one (shouldnt be possible for both to miss)
        match (s1Time <= (s2Time + this.epsilon)) with 
        |true -> 
            match (s2.isInside (r.PointAtTime s1Time)) with
            |true ->  
                let newOrigin = (r.PointAtTime s1Time).Move (r.GetDirection.MultScalar (this.epsilon))
                this.unionHitFunctionInside originalRay (new Ray(newOrigin, r.GetDirection.Normalise))//keep firing the ray (might have to move the origin forward a bit
            |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime(s1Hit.Time)), s1Hit.Normal, s1Hit.Material, this) //if the hit, is not inside s2, we have found the hitpoint
        |false -> 
            match (s1.isInside (r.PointAtTime s2Time)) with
            |true ->
                let newnewOrigin = (r.PointAtTime s2Time).Move (r.GetDirection.MultScalar (this.epsilon))
                this.unionHitFunctionInside originalRay (new Ray(newnewOrigin, r.GetDirection.Normalise))//keep firing the ray (might have to move the origin forward a bit
            |false ->
                HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime(s2Hit.Time)), s2Hit.Normal, s2Hit.Material, this) //if the hit, is not inside s1, we have found the hitpoint

    member this.unionHitFunction (r:Ray) = match this.isInside r.GetOrigin with
                                           |false -> 
                                                let s1Hit = s1.hitFunction r
                                                let s2Hit = s2.hitFunction r
                                                match ((s1Hit.DidHit) || (s2Hit.DidHit)) with // this check reduced the render time of 10 tri-Unions (3 solid cylinders), from 41.5 to 28 sek!!!
                                                |true ->
                                                    let s1Time = match s1Hit.DidHit with
                                                                 |true -> s1Hit.Time 
                                                                 |false -> infinity
                                                    let s2Time = match s2Hit.DidHit with
                                                                 |true -> s2Hit.Time 
                                                                 |false -> infinity
                                                    match (s1Time <= (s2Time + this.epsilon)) with
                                                    |true -> HitPoint(r, s1Hit.Time, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                                    |false -> HitPoint(r, s2Hit.Time, s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                                                |false -> HitPoint(r)
                                           |true -> this.unionHitFunctionInside r r
                                               

    ////INTERSECTION////
    member this.intersectionHitFunction (originalRay:Ray) (r:Ray) = 
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r
        match (s1Hit.DidHit || s1Hit.DidHit) with //check if any hit was found
        |true ->
            let s1Time = match s1Hit.DidHit with
                            |true -> s1Hit.Time
                            |false -> infinity
            let s2Time = match s2Hit.DidHit with
                            |true -> s2Hit.Time
                            |false -> infinity
        

            match (s1Time, s2Time) with
            |(s1T, s2T) when s1T = infinity && s2T = infinity -> HitPoint(r) //if the ray misses
            |(s1T, s2T) when s1T = infinity -> match (s1.isInside (r.PointAtTime s2T)) with
                                                |true -> 
                                                    HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s2Hit.Time), s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                                                |false -> 
                                                    let newOrigin = (r.PointAtTime s2T).Move (r.GetDirection.MultScalar (this.epsilon))
                                                    this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection.Normalise))
            |(s1T, s2T) when s2T = infinity -> match (s2.isInside (r.PointAtTime s1T)) with
                                                |true -> 
                                                    HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s1Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                                |false -> 
                                                    let newOrigin = (r.PointAtTime s1T).Move (r.GetDirection.MultScalar (this.epsilon))
                                                    this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection.Normalise))
            |(s1T, s2T) when (s2T - this.epsilon) < s1T && s1T < (s2T + this.epsilon) -> 
                HitPoint(originalRay, s1Hit.Time, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
            |(s1T, s2T) -> 
                        //hit function, that fires rays fom the furthest hit, instead of the closest, might provide speed increase for more complex csg
                        match (s1T > s2T) with
                        |true -> 
                            match (s2.isInside (r.PointAtTime s1T)) with //might be able to condense this with next match
                            |true ->  
                                match (s1.isInside (r.PointAtTime s2T)) with
                                |true -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s2Hit.Time), s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                                |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s1Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                            |false -> 
                                match (s1.isInside (r.PointAtTime s2T)) with
                                |true -> s2Hit
                                |false ->
                                    let newOrigin = (r.PointAtTime s1T).Move (r.GetDirection.MultScalar (this.epsilon))
                                    this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection.Normalise))
                        |false -> 
                            match (s1.isInside (r.PointAtTime s2T)) with //might be able to condense this with next if
                            |true ->
                                match (s2.isInside (r.PointAtTime s1T)) with
                                |true -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s1Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s2Hit.Time), s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                            |false ->
                                match (s2.isInside (r.PointAtTime s1T)) with
                                |true -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s2Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                |false -> 
                                    let newOrigin = (r.PointAtTime s2T).Move (r.GetDirection.MultScalar (this.epsilon))
                                    this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection.Normalise))
        |false -> HitPoint(r)


    ////SUBTRACTION////
    member this.subtractionHitFunction (originalRay:Ray) (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at first shape
   
        match s1Hit.DidHit with 
        |true -> 
            match ( s2.isInside (r.PointAtTime (s1Hit.Time))) with
            |true -> //refire Ray
                let newOrigin = (r.PointAtTime s1Hit.Time).Move (r.GetDirection.MultScalar (this.epsilon))
                let r2 = new Ray(newOrigin, r.GetDirection.Normalise) //make new ray, so you dont repeat hits
                let s2Hit = s2.hitFunction r2 //fire new ray at second shape

                match s2Hit.DidHit with //can it even not hit s2, after i make a new ray with origin inside s2?
                |true ->
                    match s1.isInside (r2.PointAtTime (s2Hit.Time)) with 
                    |true ->                                                                       
                        HitPoint(originalRay, originalRay.TimeAtPoint(r2.PointAtTime s2Hit.Time), s2Hit.Normal.Invert, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                    |false -> 
                        let newnewOrigin = (r2.PointAtTime s2Hit.Time).Move (r2.GetDirection.MultScalar (this.epsilon))
                        this.subtractionHitFunction originalRay (new Ray(newnewOrigin, r2.GetDirection.Normalise)) //the direction vector should be the same for r and r2
                |false -> HitPoint(r)
            |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime s1Hit.Time), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
        |false -> HitPoint(r)
    
    
    ////GROUPING////
    member this.groupingHitFunction (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r

        match (s1Hit.DidHit || s2Hit.DidHit) with
        |true ->
            let s1Time = match s1Hit.DidHit with
                         |true -> s1Hit.Time 
                         |false -> infinity
            let s2Time = match s2Hit.DidHit with 
                         |true -> s2Hit.Time 
                         |false -> infinity

            match (s1Time <= (s2Time + this.epsilon)) with 
            |true -> HitPoint(r, s1Hit.Time, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
            |false -> HitPoint(r, s2Hit.Time, s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
        |false -> HitPoint(r)

    
    ////GENERAL HIT-FUNCTION////
    override this.hitFunction (r:Ray) = match op with
                                        |Union -> this.unionHitFunction r
                                        |Intersection -> this.intersectionHitFunction r r //the extra ray is because they need the original ray, to calculate correct t-value
                                        |Subtraction -> this.subtractionHitFunction r r //the extra ray is because they need the original ray, to calculate correct t-value
                                        |Grouping -> this.groupingHitFunction r


