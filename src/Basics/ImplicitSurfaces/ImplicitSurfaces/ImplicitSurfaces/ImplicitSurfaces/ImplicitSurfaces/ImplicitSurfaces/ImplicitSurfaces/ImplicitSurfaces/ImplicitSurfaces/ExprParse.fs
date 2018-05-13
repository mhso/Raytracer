namespace Tracer

module ExprParse =

  (* Grammar:

  E    = T Eopt .
  Eopt = "+" T Eopt | e .
  T    = F Topt .
  Topt = "*" F Topt | e .
  F    = P Fopt .
  Fopt = "^" Int | e .
  P    = Int [ Float | Var | "(" E ")" .

  e is the empty sequence.
  *)

  type terminal = 
    | Add               // addition
    | Mul               // multiplication
    | Div               // Division
    | Pwr               // Power
    | Root              // Root    
    | Lpar              // Left parenthesis
    | Rpar              // Right parenthesis
    | Int of int        // Wrapped value of integer
    | Float of float    // Wrapped value of float
    | Var of string     // Wrapped variable of string

  let isblank c = System.Char.IsWhiteSpace c
  let isdigit c  = System.Char.IsDigit c
  let isletter c = System.Char.IsLetter c
  let isletterdigit c = System.Char.IsLetterOrDigit c

  let explode s = [for c in s -> c]

  let floatval (c:char) = float((int)c - (int)'0')
  let intval (c:char) = (int)c - (int)'0'

  exception ScanErrorException

  let rec scnum (cs, value) = 
    match cs with 
    | '.' :: c :: cr when isdigit c       -> scfrac(c :: cr, (float)value, 0.1)
    | c :: cr when isdigit c              -> scnum(cr, 10* value + intval c)
    | _                                   -> (cs,Int value) // Number without fraction is an integer
  and scfrac (cs, value, wt) =
    match cs with
    | c :: cr when isdigit c  -> scfrac(cr, value+wt*floatval c, wt/10.0)
    | _                       -> (cs, Float value)

  let rec scname (cs, value) =
    match cs with
    | c :: cr when isletterdigit c  -> scname(cr, value + c.ToString())
    | _                             -> (cs, value)

  let scan s =
    let negateNumber = function
        Int i -> Int -i
      | Float f -> Float -f
      | _ -> raise ScanErrorException // Expected a number
    let rec sc cs = 
      match cs with
      | []                              -> []
      | '+' :: cr                       -> Add :: sc cr
      | '*' :: cr                       -> Mul :: sc cr
      | '^' :: cr                       -> Pwr :: sc cr
      | '/' :: cr                       -> Div :: sc cr
      | '(' :: cr                       -> Lpar :: sc cr
      | ')' :: cr                       -> Rpar :: sc cr
      | '_' :: cr                       -> Root :: sc cr
      //| '-' :: c :: cr when isdigit c   -> let (cs1, t) = scnum(cr, intval c)
      //                                     negateNumber t :: sc cs1
      //| '-' :: c1 :: c2 :: cr when isblank c1 && isdigit c2 ->
      //                                     let (cs1, t) = scnum(cr, intval c2)
      //                                     Add :: negateNumber t :: sc cs1 
      | '-' :: cr                       -> Add :: Float -1.0 :: Mul :: sc cr // Subtraction and negation is treated as multiplied by minus 1//Sub :: sc cr
      //Add :: Float -1.0 :: Mul :: sc cr // Subtraction and negation is treated as multiplied by minus 1
      | c :: cr when isdigit c          -> let (cs1, t) = scnum(cr, intval c)
                                           t :: sc cs1
      | c :: cr when isblank c          -> sc cr
      | c :: cr when isletter c         -> let (cs1, n) = scname(cr, (string)c)
                                           Var n :: sc cs1
      | _                               -> raise ScanErrorException
    sc (explode s)

  // trying to use active patterns
  let (|Lterm|NoMatch|) left = 
    match left with
    | Float _ | Var _ | Int _ -> Lterm left
    | _                       -> NoMatch
  let (|Rterm|NoMatch|) right =
    match right with
    | Float _ | Var _ | Int _ | Lpar _  -> Rterm right
    | _                                 -> NoMatch

  let rec insertMult = function
    | Lterm left::Rterm right::ts -> left::Mul::insertMult (right::ts)
    | t::ts                       -> t::insertMult ts
    | []                          -> []

  type expr = 
    | FNum of float
    | FVar of string
    | FAdd of expr * expr
    | FMult of expr * expr
    | FDiv of expr * expr
    | FExponent of expr * int
    | FRoot of expr * int

  exception ParseErrorException

  let rec E (ts:terminal list) = (T >> Eopt) ts // or Eopt (T ts), and the full composition translates to Eopt (Topt (Fopt (P ts)))
  and Eopt (ts, (inval)) = 
    match ts with 
    | Add::tr   -> let (ts1, tv) = T tr
                   Eopt (ts1, FAdd (inval, tv))
    | _         -> (ts, inval)
  and T ts = (F >> Topt) ts // or Topt (F ts)
  and Topt (ts, inval) =
    match ts with
    | Mul::tr   -> let (ts1, fv) = F tr // fv = factor value?
                   Topt (ts1, FMult (inval, fv))
    | Div::tr   -> let (ts1, fv) = F tr
                   Topt (ts1, FDiv (inval, fv))                       
    | _         -> (ts, inval)          
  and F ts = (P >> Fopt) ts // or Fopt (P ts)
  and Fopt (ts, inval) =
    match ts with
    | Pwr::Int i::tr  -> (tr, FExponent (inval, i))
    | Root::Int i::tr -> (tr, FRoot (inval, i))
    | _               -> (ts, inval)                    
  and P ts = // this function is executed first?
    match ts with
    | Float r::tr -> (tr, FNum r)
    | Int i::tr   -> (tr, FNum (float i))
    | Var x::tr   -> (tr, FVar x)
    | Lpar::tr    -> let (ts1, ev) = E tr
                     match ts1 with
                     | Rpar :: tr -> (tr, ev)
                     | _          -> raise ParseErrorException
    | Add::tr -> P tr 
    | _           -> raise ParseErrorException

  let parse ts : expr= 
    match E ts with
    | ([], result)  -> result
    | _             -> raise ParseErrorException

  let rec reduceExpr e =
    let rec inner = function
      // remove zero-terms
      | FAdd(e1, FNum 0.0) -> inner e1
      | FAdd(FNum 0.0, e1) -> inner e1
      | FMult(FNum 0.0, _) -> FNum 0.0
      | FMult(_, FNum 0.0) -> FNum 0.0
      | FDiv(FNum 0.0, _)  -> FNum 0.0
      | FDiv(_, FNum 0.0)  -> raise ParseErrorException
      // some small simplifications with numbers
      | FAdd(FNum c1, FNum c2)  -> FNum (c1 + c2)
      | FMult(FNum c1, FNum c2) -> FNum (c1 * c2)
      | FRoot(FNum c1, n)       -> FNum (c1**(1. / (float n)))
      | FDiv(FNum c1, FNum c2)  -> FNum (c1 / c2)
      | FExponent(FNum c1,n)    -> FNum (pown c1 n)
      // all others should just continue recursively
      | FRoot(e1,n)        -> FRoot (inner e1, n)
      | FAdd(e1,e2)        -> FAdd (inner e1, inner e2)
      | FMult(e1,e2)       -> FMult (inner e1, inner e2)
      | FDiv(e1,e2)        -> FDiv (inner e1, inner e2)
      | FExponent(e1,n)    -> FExponent (inner e1, n)
      | ex                 -> ex // FVar FNum
    let altered = inner e
    if e = altered then e
    else reduceExpr altered

  let parseStr s = (scan >> insertMult >> parse) s

  // requires a map of all the variables used in the expression, mapped to float values
  let rec solveExpr m = function
  | FNum c          -> c
  | FVar s          -> match Map.tryFind s m with
                       | Some v -> v
                       | None   -> failwith "solveExpr: variable not found"
  | FRoot(e1,n)     -> (solveExpr m e1)**(1. / (float n))
  | FAdd(e1,e2)     -> solveExpr m e1 + solveExpr m e2
  | FMult(e1,e2)    -> solveExpr m e1 * solveExpr m e2
  | FDiv(e1,e2)     -> solveExpr m e1 / solveExpr m e2
  | FExponent(e1,n) -> pown (solveExpr m e1) n

  let dotAST ast =
    let fixStr (s:string) = s.Replace ("\"", "\\\"")
    let genDot s n e = "digraph G {\nlabel=\"" + (fixStr s) + "\"\n" + n + e + "\n}"
    // i is unique label such that nodes and edges are unique in DiGraph.
    let genNodeStr i l = "Node"+(string i)+" [label=\""+l+"\"];\n"
    let genEdgeStr i1 i2 = "Node"+(string i1)+" -> " + "Node"+(string i2)+";\n"
    // Edges are unique and stored in a set.
    // Nodes are not unique and stored in a map, i.e., node with "+" may happen several times. 
    let rec genNE (i,nmap,eset) = function
      FNum r -> (i,Map.add i (genNodeStr i ((string)r)) nmap,eset)            // Add node with number
    | FVar x -> (i,Map.add i (genNodeStr i x) nmap,eset)                      // Add node with variable
    | FAdd (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1         // Generate nodes and edges for e1 and e2
                      let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
                      (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "+") nmap2,                      // Add node for "+"
                       Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "+"->e1 and "+"->e2
    | FMult (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1        // Generate nodes and edges for e1 and e2
                       let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
                       (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "*") nmap2,                      // Add node for "*"
                        Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "*"->e1 and "*"->e2
    | FExponent (e1,ie) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1                                // Generate nodes and edges for e1
                           let (i2,nmap2) = (i1+1,Map.add (i1+1) (genNodeStr (i1+1) ((string)ie)) nmap1)  // Add node for integer (exponent)
                           (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "^") nmap2,                            // Add node for "^"
                            Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset1))        // Add edges for "^"->e1 and "^"->ie
    | _ -> failwith "the rest of expr types not handled here"
    let (_,nmap,eset) = genNE (0,Map.empty,Set.empty) ast  // Generate map for nodes and set for edges
    genDot (sprintf "%A\n" ast) (Map.fold (fun acc _ s -> acc + s) "" nmap) (Set.fold (+) "" eset)  // Generate big string with dot-code.
    
