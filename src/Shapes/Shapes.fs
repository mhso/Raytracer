namespace Tracer


type material = NotImplementedException

type texture = NotImplementedException



type baseShape = (Point*Vector) -> (Point option*Vector option)

[<AbstractClass>]
type Shape()=
    abstract member hitFunction: Ray -> float option*Vector option*texture option




type Rectangle(bottomLeft:Point, topLeft:Point, bottomRight:Point, tex:texture)=
    inherit Shape()
    member this.bottomleft = bottomLeft
    member this.topLeft = topLeft
    member this.bottomRight = bottomRight
    member this.tex = tex
    member this.width = bottomRight.X - bottomLeft.X
    member this.height = topLeft.Y - bottomLeft.Y
    override this.hitFunction (r:Ray) = match r with
                                             |(r) when (r.GetDirection.Z) = 0.0 -> (None, None, None) //This method checks if dz = 0.0, which would make the ray, parrallel to the plane 
                                             |(r) when (-((r.GetOrigin.Z) / (r.GetDirection.Z))) <= 0.0 -> (None, None, None) //This checks if t is 0 or smaller, in which case there is no hit
                                             |(r) -> let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z)))
                                                     let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                                                     let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                                                     if (px > 0.0 && px < this.width) && (py > 0.0 && py < this.height) 
                                                        then (Some(t),Some(new Vector(0.0, 0.0, 1.0)),Some(tex)) else (None, None, None)


                                                        
type Disc(center:Point, radius:float, tex:texture)=
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.tex = tex
    override this.hitFunction (r:Ray) = match r with
                                             |(r) when (r.GetDirection.Z) = 0.0 -> (None, None, None) //This method checks if dz = 0.0, which would make the ray, parrallel to the plane 
                                             |(r) when (-((r.GetOrigin.Z) / (r.GetDirection.Z))) <= 0.0 -> (None, None, None) //This checks if t is 0 or smaller, in which case there is no hit
                                             |(r) -> let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z)))
                                                     let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                                                     let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                                                     if (((px*px)+(py*py)) <= radius*radius) 
                                                         then (Some(t),Some(new Vector(0.0, 0.0, 1.0)),Some(tex)) else (None, None, None) //needs to return texture somehow
    



type Triangle(a:Point, b:Point, c:Point, mat:material)=
    inherit Shape()
    member this.a = a
    member this.b = b
    member this.c = c
    member this.mat = mat
    member this.u = a-b //in case of errors try swithing a and b around
    member this.v = a-c // same here

    //this is to simplify the discriminant hit calc, so its actually somewhat readable... 
    member this.pa = ((a.X)-(b.X))
    member this.pb = ((a.X)-(c.X))
    member this.e = ((a.Y)-(b.Y))
    member this.f = ((a.Y)-(c.Y))
    member this.i = ((a.Z)-(b.Z))
    member this.j = ((a.Z)-(c.Z))

    //the man let statements are fo simplifying cramers rule
    override this.hitFunction (r:Ray) = let pa = ((a.X)-(b.X))
                                        let pb = ((a.X)-(c.X))
                                        let e = ((a.Y)-(b.Y))
                                        let f = ((a.Y)-(c.Y))
                                        let i = ((a.Z)-(b.Z))
                                        let j = ((a.Z)-(c.Z))

                                        match r with
                                        |(r) when (pa*((f*(r.GetDirection.Z))-((r.GetDirection.Y)*j)) + pb*(((r.GetDirection.Y)*i)-(e*(r.GetDirection.Z))) + (r.GetDirection.X)*((e*j)-(f*i))) = 0.0 -> (None, None, None)
                                        |(r) -> let pc = (r.GetDirection.X)
                                                let g = (r.GetDirection.Y)
                                                let k = (r.GetDirection.Z)
                                                let d = ((a.X)-(r.GetOrigin.X)) 
                                                let h = ((a.Y)-(r.GetOrigin.Y)) 
                                                let l = ((a.Z)-(r.GetOrigin.Z))
                                                let D = (pa*((f*k)-(g*j)) + pb*((g*i)-(e*k)) + pc*((e*j)-(f*i)))
                                                let x = (d*((f*k)-(g*j)) + pb*((g*l)-(h*k)) + pc*((h*j)-(f*l)))/D
                                                let y = (pa*((h*k)-(g*l)) + d*((g*i)-(e*k)) + pc*((e*l)-(h*i)))/D
                                                let z = (pa*((f*l)-(h*j)) + pb*((h*i)-(e*l)) + d*((e*j)-(f*i)))/D
                                                //x=beta, y=gamma, z=t
                                                //alpha is gained from 1-x-y, this is used for texturing (alpha, beta, gamma that is)
                                                if (x <= 1.0 && x >= 0.0) && (y <= 1.0 && y >= 0.0) && (x+y <= 1.0 && x+y >= 0.0) && (z>0.0)
                                                      then (Some(z), Some((this.u % this.v).Normalise), Some(mat)) else (None, None, None)



// these needs to be moved to API somehow, but i have weird problems with it...
module Shapes

let mkRectangle (bottomLeft:Point) (topLeft:Point) (bottomRight:Point) (tex:texture) = new Rectangle(bottomLeft, topLeft, bottomRight, tex)

let mkDisc (center:Point) (radius:float) (tex:texture) = new Disc(center, radius, tex)

let mkTriangle (a:Point) (b:Point) (c:Point) (mat:material) = new Triangle(a, b, c, mat)


let mkShape (bShape:baseShape) (t:texture) = function
    | _  -> NotImplementedException
  












