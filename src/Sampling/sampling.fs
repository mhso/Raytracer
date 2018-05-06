module Tracer.Sampling.Sampling

open System
open System.Drawing
open System.Threading

let mutable rand = new Random()

let setRandomSeed seed = rand <- new Random(seed)

type SampleGenerator(samplingAlgorithm: int -> int -> (float * float) [][], sampleSize: int, sampleSetCount: int) =   
    let samples: (float * float) [][] = samplingAlgorithm sampleSize sampleSetCount

    let mutable currentSampleIndex = 0
    let mutable currentSample: (float * float) = (0., 0.)
    let sampleCount = samples.[0].Length

    member this.Next() = 
        let setIndex = (currentSampleIndex / sampleCount) % sampleSetCount
        let sampleIndex = currentSampleIndex % sampleCount
        let sample = samples.[setIndex].[sampleIndex]
        currentSampleIndex <- currentSampleIndex + 1
        currentSample <- sample
        sample

    member this.Current = 
        currentSample

    member this.SampleCount = sampleCount

let drawSamples (sl:(float * float) []) sampleMethod fileName =
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

let drawCircle (img:Bitmap) size = 
    let SIZE_HALVED = float size/2.0
    for i in 1..int 360 do
        let theta = float i
        let x  =  int(SIZE_HALVED + (SIZE_HALVED-2.0) * Math.Cos(theta))
        let y  =  int(SIZE_HALVED + (SIZE_HALVED-2.0) * Math.Sin(theta))
        for i in [x-1..x+1] do
            for j in [y-1..y+1] do
                img.SetPixel(i, j, Color.Black)

let drawDiscSamples  (sl:(float * float) []) fileName =
    let size = 400
    let dotSize = 4
    let img = new Bitmap(size, size)
    for i in 0..size-1 do
            for j in [0..size-1] do
                img.SetPixel(i, j, Color.White)
    drawCircle img size
    for (sx, sy) in sl do
        let x = int (float (size)*((sx+1.0)/2.0))
        let y = int (float (size)*((sy+1.0)/2.0))
        for i in [x-(dotSize/2)..x+(dotSize/2)] do
            for j in [y-(dotSize/2)..y+(dotSize/2)] do
                if i >= 0 && j >= 0 && i < size && j < size then img.SetPixel(i, j, Color.Red)
    img.Save(fileName)  

let drawSphereSamples (sl:(float * float * float) []) fileName above =
    let size = 400
    let dotSize = 4
    let img = new Bitmap(size, size)
    for i in 0..size-1 do
            for j in [0..size-1] do
                img.SetPixel(i, j, Color.White)
    drawCircle img size
    for (sx, sy, sz) in sl do
        let x = int (float (size)*((sx+1.0)/2.0))
        let sv = if above then sy else sz
        let y = int (float (size)*((sv+1.0)/2.0))
        for i in [x-(dotSize/2)..x+(dotSize/2)] do
            for j in [y-(dotSize/2)..y+(dotSize/2)] do
                if i >= 0 && j >= 0 && i < size && j < size then img.SetPixel(i, j, Color.Red)
    img.Save(fileName)

let regular (ni:int) =
    let n = float ni
    let samples = Array.create ni (0.0, 0.0)
    let rec innerX x = 
        let rec innerY = function
            | 0 -> samples.[0] <- (float x/(n+1.0), 1.0/(n+1.0))
            | y -> 
                samples.[y] <- (float x/(n+1.0), float y/(n+1.0))
                (innerY (y-1))
        match x with
        | 0 -> innerY ni
        | c ->
            (innerY ni)
            (innerX (c-1))
    innerX (ni-1)
    [|samples|]

let createSampleSets (set:(float * float)[][]) =
    let rand = new Random() // We create a new Random here, for help with testing.
    for i in 0..set.Length-1 do
        let samples = set.[i]
        for j in 1..samples.Length-1 do
            let r = rand.Next(j)
            let temp = samples.[r]
            samples.[r] <- samples.[j]
            samples.[j] <- temp
        set.[i] <- samples
    for i in 1..set.Length-1 do
            let r = rand.Next(i)
            let temp = set.[r]
            set.[r] <- set.[i]
            set.[i] <- temp
    set

let random n sn =
    let ns = n*n
    let sets = Array.create sn [|(0.0, 0.0)|]
    let samples = Array.create ns (0.0, 0.0)
    let rec loop k =
        let rec inner = function
            | 0 -> samples.[0] <- (rand.NextDouble(), rand.NextDouble())
            | c -> 
                samples.[c] <- (rand.NextDouble(), rand.NextDouble())
                inner (c-1)
        inner (ns-1)
        sets.[k] <- samples
        if k > 0 then loop (k-1)
    loop (sn-1)
    createSampleSets sets

let getJitteredValue (grid:int) (max:int) = (rand.NextDouble()/float max) + ((1.0/float max) * float grid)

let jittered n sn =
    let sets = Array.create sn [|(0.0, 0.0)|]
    let samples = Array.create (int (float n**2.0)) (0.0 ,0.0)
    let rec loop k =
        let rec innerX x = 
            let rec innerY = function
                | 0 -> samples.[x*n] <- (getJitteredValue x n, getJitteredValue 0 n)
                | y -> 
                    samples.[(x*n) + y] <- (getJitteredValue x n, getJitteredValue y n)
                    (innerY (y-1))
            match x with
            | 0 -> innerY (n-1)
            | c ->  
                innerY (n-1)
                (innerX (c-1))
        innerX (n-1)
        sets.[k] <- samples
        if k > 0 then loop (k-1)
    loop (sn-1)
    createSampleSets sets

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

let shuffleDiagonals (samples:(float * float) []) =
    let n = samples.Length
    
    for i in n-1..-1..0 do
        let shufX = rand.Next(i)
        let _, replY = samples.[shufX]
        let  _, currentY = samples.[i]
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
    samples

let nRooks n sn =
    let sets = Array.create sn [|(0.0, 0.0)|]
    let samples = Array.create n (0.0,0.0)
    let rec loop k =
        let rec placeDiagonals = function
            | 0 -> samples.[0] <- (getJitteredValue 0 n, getJitteredValue 0 n)
            | c -> 
                samples.[c] <- (getJitteredValue c n, getJitteredValue c n)
                (placeDiagonals (c-1))
        placeDiagonals (n-1)
        sets.[k] <- shuffleDiagonals samples
        if k > 0 then loop (k-1)
    loop (sn-1)
    createSampleSets sets

let shuffleMultiPDF (samples:(float * float) []) n =
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

    samples

let shuffleMulti (samples:(float * float) []) n =
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
    samples

let multiJittered n sn =
    let sets = Array.create sn [|(0.0, 0.0)|]
    let rec loop k = 
        let ns = int (float (n)**2.0)
        let samples = Array.create ns (0.0, 0.0)
        let rec placeDiag = function
            | 0 -> samples.[0] <- (getJitteredValue 0 ns, getJitteredValue 0 ns)
            | c -> 
                samples.[c] <- (getJitteredValue (((c%n)*n)+(c/n)) ns, getJitteredValue c ns)
                placeDiag (c-1)
        placeDiag (ns-1)
        sets.[k] <- shuffleMulti samples n
        if k > 0 then loop (k-1)
    loop (sn-1)
    createSampleSets sets

let mapToDisc (x, y) =
    let x, y = (2.0*x-1.0, 2.0*y-1.0)
    let PI_QUART = Math.PI/4.0
    let (r, theta) = 
        match (x > -y, x > y) with
            | true, true    -> (x, PI_QUART * (y/x))
            | true, false   -> (y, PI_QUART * (2.0-x/y))
            | false, false  -> (-x, PI_QUART * (4.0+y/x))
            | false, true   -> (-y, PI_QUART * (6.0-x/y))
    (r * Math.Cos(theta), r*Math.Sin(theta))
    
let mapToHemisphere (x, y) e =
    let E_VAL = 1.0/(e+1.0)
    let phi = 2.0*Math.PI*x
    let theta = Math.Acos((1.0-y)**E_VAL)
    (Math.Sin(theta) * Math.Cos(phi), Math.Sin(theta) * Math.Sin(phi), Math.Cos(theta))

(*for i in 0..1920/127 do
    for j in 0..1080/127 do
        ignore (nRooks 256 127)*)

[<EntryPoint>]
let main argsv =
    if Array.isEmpty argsv then 
        printfn "Error: No arguments given! Expected: [sampleMethod] [sampleAmount]."
        0
    else
    let method = argsv.[0]
    let amount = Int32.Parse(argsv.[1])
    let sets = if argsv.Length > 2 then Int32.Parse(argsv.[2]) else 1
    let fileName = "sampletest.png"
    let samples = 
        match method with
            | "regular" -> (regular amount).[0]
            | "random"  -> (random amount sets).[0]
            | "jittered" -> (jittered amount sets).[0]
            | "nrooks"  -> (nRooks amount sets).[0]
            | "multi"   -> (multiJittered amount sets).[0]
            | _ -> (regular 4).[0]
    if argsv.Length > 3 then
        if argsv.[3] = "disc" then drawDiscSamples (Array.map mapToDisc samples) fileName
        else if argsv.[3] = "sphere" then
            let e = if argsv.Length = 5 then float (Int32.Parse argsv.[4]) else 0.0
            drawSphereSamples (Array.map (fun (x, y) -> (mapToHemisphere (x, y) e)) samples) fileName true
    else drawSamples samples method fileName
    0