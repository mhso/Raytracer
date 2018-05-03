module RegularGrids

open System

let clamp (x:float,b:float) =
    match x with
    | x when x<0. -> 0.
    | x when x>b -> b
    | _ -> x

let calcEdgeLength (wx:float) (wy:float) (wz:float) (n:float) : float = System.Math.Pow (((wx*wy*wz)/n),(1./3.))

let calcAxisCell (m:float) (w:float) (s:float) : float = System.Math.Floor ((m*w)/s)+1.

let calcAxisCells (wx:float) (wy:float) (wz:float) (m:float) (n:float) = 
    let s =  calcEdgeLength wx wy wz n
    let nx = calcAxisCell m wx s
    let ny = calcAxisCell m wy s
    let nz = calcAxisCell m wz s
    (nx, ny, nz)

    