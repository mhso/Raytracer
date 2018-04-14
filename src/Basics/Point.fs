namespace Tracer.Basics

type Point(x:float, y:float, z:float) = 
    
    //- PRIVATE FIELDS
    let x = x
    let y = y
    let z = z

    //- PUBLIC FIELDS
    member this.X = x
    member this.Y = y
    member this.Z = z
    
    //- PUBLIC METHODS
    override this.ToString() = 
        "("+x.ToString()+","+y.ToString()+","+z.ToString()+")"
    member this.GetCoord = (x,y,z)
    member this.Move (v: Vector) = new Point(x+v.X, y+v.Y, z+v.Z)
    member this.Distance (q: Point) = new Vector((q.X-x),(q.Y-y),(q.Z-z))
    member this.Direction (p:Point) (q:Point) = p.Distance(q).Normalise
    member this.Round (d:int) = new Point(System.Math.Round(x,d), System.Math.Round(y,d), System.Math.Round(z,d))
    member this.ToVector = new Vector(x,y,z)
    static member ( + ) (p:Point, v: Vector) : Point = p.Move v 
    static member ( - ) (p:Point, v: Vector) : Point = 
        p.Move v.Invert
    static member ( - ) (p1: Point, p2: Point) = p1.Distance(p2)
    static member ( - ) (v:Vector, p:Point) : Vector = 
        new Vector (((v.X) - (p.X)), ((v.Y) - (p.Y)), ((v.Z) - (p.Z)))
    static member ( - ) (p: Point,n:float) = new Point((p.X-n), (p.Y-n), (p.Z-n))
