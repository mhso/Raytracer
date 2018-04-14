module Sampling

open System
open System.Drawing

let rand = new Random()

let drawSamples (sl:(float * float) list) sampleMethod fileName =
    let size = 400
    let dotSize = 4
    let img = new Bitmap(size, size)
    for i in [0..size-1] do
            for j in [0..size-1] do
                img.SetPixel(i, j, Color.White)
    let drawGrid n thickness =
        for i in [1..n-1] do
            for j in [0..size-1] do
                for k in [(-thickness/2)..(thickness/2)] do
                    img.SetPixel(((size/n)*i)+k, j, Color.Black)
                    img.SetPixel(j, ((size/n)*i)+k, Color.Black)
    if sl.Length < 64 then
        if sampleMethod = "jittered" then drawGrid (int (Math.Sqrt(float sl.Length))) 1
        else if sampleMethod = "nrooks" then drawGrid sl.Length 1
        else if sampleMethod = "multi" then
            drawGrid (int (Math.Sqrt(float sl.Length))) 2
            drawGrid sl.Length 1
    for (sx, sy) in sl do
        let x = int (float size*sx)
        let y = int (float size*sy)
        for i in [x-(dotSize/2)..x+(dotSize/2)] do
            for j in [y-(dotSize/2)..y+(dotSize/2)] do
                if i >= 0 && j >= 0 && i < size && j < size then img.SetPixel(i, j, Color.Red)
    img.Save(fileName)  

let drawDiscSamples  (sl:(float * float) list) fileName =
    let size = 400
    let dotSize = 4
    let img = new Bitmap(size, size)
    for i in 0..size-1 do
            for j in [0..size-1] do
                img.SetPixel(i, j, Color.White)
    let SIZE_HALVED = float size/2.0
    for i in 1..int 360 do
        let theta = float i
        let x  =  int(SIZE_HALVED + (SIZE_HALVED-2.0) * Math.Cos(theta))
        let y  =  int(SIZE_HALVED + (SIZE_HALVED-2.0) * Math.Sin(theta))
        for i in [x-1..x+1] do
            for j in [y-1..y+1] do
                img.SetPixel(i, j, Color.Black)
    for (sx, sy) in sl do
        let x = int (float (size)*((sx+1.0)/2.0))
        let y = int (float (size)*((sy+1.0)/2.0))
        for i in [x-(dotSize/2)..x+(dotSize/2)] do
            for j in [y-(dotSize/2)..y+(dotSize/2)] do
                if i >= 0 && j >= 0 && i < size && j < size then img.SetPixel(i, j, Color.Red)
    img.Save(fileName)  

let regular (ni:int) =
    let n = float ni
    let rec innerX x = 
        let rec innerY = function
            | 1.0 -> [(x/(n+1.0), 1.0/(n+1.0))]
            | y -> (x/(n+1.0), y/(n+1.0))::(innerY (y-1.0))
        match x with
        | 1.0 -> innerY n
        | c -> (innerY n) @ (innerX (c-1.0))
    innerX n

let random n =
    let rec inner = function
        | 1.0 -> [(rand.NextDouble(), rand.NextDouble())]
        | c -> (rand.NextDouble(), rand.NextDouble())::inner (c-1.0)
    inner (float (n*n))

let getJitteredValue (grid:int) (max:int) = (rand.NextDouble()/float max) + ((1.0/float max) * float grid)

let jittered n =
    let rec innerX x = 
        let rec innerY = function
            | 0 -> [(getJitteredValue x n, getJitteredValue 0 n)]
            | y -> (getJitteredValue x n, getJitteredValue y n)::(innerY (y-1))
        match x with
        | 0 -> innerY n
        | c ->  (innerX (c-1))@(innerY n)
    innerX (n-1)

let getGrid (v:float) max = int(v*(float max))

let illegalSpots = [|(2, 1);(-2, 1);(2, -1);(-2, -1);(1, 2);(1, -2);(-1, 2);(-1, -2)|]

let rec getLegalSpots i (samples:(float * float) array) =
    let n = samples.Length
    
    let mutable result = [||]
    for j in 0..i-1 do
        let xVal, _ = samples.[j]
        let xGrid = getGrid xVal n
        let checkValidity k =
            if k < n && k > 0 then
                let (x2, y2) = samples.[k]
                let xGrid2 = getGrid x2 n
                let yGrid2 = getGrid y2 n
                not (Array.exists (fun (xi, yi) ->
                    xGrid2 = (xGrid+xi) && yGrid2 = (i+yi)) illegalSpots)
            else true
        result <- if checkValidity (i-1) && checkValidity (i-2) && checkValidity (i+1) && checkValidity (i+2) then Array.append result [|j|] else result
    result

let shuffleDiagonals (sampleList:(float * float) list) =
    let samples = (List.toArray sampleList)
    let n = sampleList.Length
    
    for i in n-1..-1..0 do
        let k = n-1-i
        let shufX = rand.Next(i)
        let _, replY = samples.[shufX]
        let _, currentY = samples.[i]
        samples.[shufX] <- (getJitteredValue shufX n, currentY)
        samples.[i] <- (getJitteredValue i n, replY)
    for i in n-1..-1..0 do
        let legalSpots = getLegalSpots i samples
        if not (Array.isEmpty legalSpots) then
            let shufY = legalSpots.[rand.Next(legalSpots.Length)]
            let replX, _ = samples.[shufY]
            let currentX, _ = samples.[i]
            samples.[shufY] <- (currentX, getJitteredValue shufY n)
            samples.[i] <- (replX, getJitteredValue i n)
    List.ofArray samples

let nRooks n =
    let rec placeDiagonals = function
        | 0 -> [(getJitteredValue 0 n, getJitteredValue 0 n)]
        | c -> (getJitteredValue c n, getJitteredValue c n)::(placeDiagonals (c-1))
    shuffleDiagonals (placeDiagonals (n-1))

let shuffleMultiPDF (sampleList:(float * float) list) n =
    let samples = (List.toArray sampleList)
    let ns = sampleList.Length
    
    for j in 0..n-1 do
        for i in 0..n-1 do
            let shuf = (j + rand.Next(n-j)) * n + i
            let current = j * n + i
            let replX, replY = samples.[shuf]
            let currentX, currentY = samples.[current]
            samples.[shuf] <- (currentX, replY)
            samples.[current] <- (replX, currentY)

    for i in 0..n-1 do
        for j in 0..n-1 do
            let shuf = j * n + (i + rand.Next(n-i))
            let current = j * n + i
            let replX, replY = samples.[shuf]
            let currentX, currentY = samples.[current]
            samples.[shuf] <- (replX, currentY)
            samples.[current] <- (currentX, replY)

    List.ofArray samples

let shuffleMulti (sampleList:(float * float) list) n =
    let samples = (List.toArray sampleList)
    let ns = sampleList.Length
    
    for j in 0..n-1 do
        let k = (j + rand.Next(n-j))
        for i in 0..n-1 do
            let shuf = k * n + i
            let current = j * n + i
            let replX, replY = samples.[shuf]
            let currentX, currentY = samples.[current]
            samples.[shuf] <- (currentX, replY)
            samples.[current] <- (replX, currentY)

    for i in 0..n-1 do
        let k = (i + rand.Next(n-i))
        for j in 0..n-1 do
            let shuf = j * n + k
            let current = j * n + i
            let replX, replY = samples.[shuf]
            let currentX, currentY = samples.[current]
            samples.[shuf] <- (replX, currentY)
            samples.[current] <- (currentX, replY)

    List.ofArray samples

let multiJittered n =
    let ns = int (float (n)**2.0)
    let rec placeDiag = function
        | 0 -> [(getJitteredValue 0 ns, getJitteredValue 0 ns)]
        | c -> (getJitteredValue (((c%n)*n)+(c/n)) ns, getJitteredValue c ns)::placeDiag (c-1)
    shuffleMulti (placeDiag (ns-1)) n

let mapToDisc (sl:(float * float) list) =
    let samples = [for (x, y) in sl do yield (2.0*x-1.0, 2.0*y-1.0)]
    let PI_QUART = Math.PI/4.0
    let mapPoints (x,y) =
        let (r, theta) = 
            match (x > -y, x > y) with
                | true, true    -> (x, PI_QUART * (y/x))
                | true, false   -> (y, PI_QUART * (2.0-x/y))
                | false, false  -> (-x, PI_QUART * (4.0+y/x))
                | false, true   -> (-y, PI_QUART * (6.0-x/y))
        (r * Math.Cos(theta), r*Math.Sin(theta))
    List.map mapPoints samples

(*let stressTest = 
    for i in [0..1920] do
        for j in[0..1080] do
            multiJittered 8*)

[<EntryPoint>]
let main argsv =
    if Array.isEmpty argsv then 
        printfn "Error: No arguments given! Expected: [sampleMethod] [sampleAmount]."
        0
    else
    let method = argsv.[0]
    let amount = Int32.Parse(argsv.[1])
    let fileName = "sampletest.png"
    let samples = 
        match method with
            | "regular" -> regular amount
            | "random"  -> random amount
            | "jittered" -> jittered amount
            | "nrooks"  -> nRooks amount
            | "multi"   -> multiJittered amount
            | _ -> regular 4
    if argsv.Length > 2 && argsv.[2] = "disc" 
    then drawDiscSamples (mapToDisc samples) fileName
    else drawSamples samples method fileName
    0