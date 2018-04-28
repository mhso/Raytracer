namespace Tracer.BaseShape

open Tracer.Basics


[<AbstractClass>]
type BaseShape()=
    abstract member toShape: Material -> Shape


type BaseRectangle(bottomLeft:Point, topLeft:Point, bottomRight:Point)=
    inherit BaseShape()
    member this.bottomLeft = bottomLeft
    member this.topLeft = topLeft
    member this.bottomRight = bottomRight
    member this.width = bottomRight.X - bottomLeft.X
    member this.height = topLeft.Y - bottomLeft.Y
    override this.toShape (mat:Material) = new Rectangle(bottomLeft, topLeft, bottomRight, mat) :> Shape

                    
type BaseDisc(center:Point, radius:float)=
    inherit BaseShape()
    member this.center = center
    member this.radius = radius
    override this.toShape (mat:Material) = new Disc(center, radius, mat) :> Shape
    

type BaseTriangle(a:Point, b:Point, c:Point)=
    inherit BaseShape()
    member this.a = a
    member this.b = b
    member this.c = c
    override this.toShape (mat:Material) = new Triangle(a, b, c, mat) :> Shape


type BaseSphere(origin: Point, radius: float) = 
    inherit BaseShape()
    member this.origin = origin
    member this.radius = radius
    override this.toShape (mat:Material) = new SphereShape(origin, radius, mat) :> Shape


type BaseHollowCylinder(center:Point, radius:float, height:float) =
    inherit BaseShape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    override this.toShape (mat:Material) = new HollowCylinder(center, radius, height, mat) :> Shape
