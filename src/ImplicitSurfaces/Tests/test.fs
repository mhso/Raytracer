//type baseShape =
//  abstract mkShape = Texture -> Shape


type shape =
  abstract hf : float -> (float * int * int)

let is = { interface shape with 
              member this.hf() = 
            
            }