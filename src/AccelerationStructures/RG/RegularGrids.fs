module RegularGrids

let clamp (x:float,b:float) =
    match x with
    | x when x<0. -> 0.
    | x when x>b -> b
    | _ -> x