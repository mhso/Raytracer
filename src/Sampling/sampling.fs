﻿module Sampling

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
    let drawGrid n =
        for i in [1..n-1] do
            for j in [0..size-1] do
                img.SetPixel((size/n)*i, j, Color.Black)
                img.SetPixel(j, (size/n)*i, Color.Black)
    if sl.Length < 64 then            
        if sampleMethod = "jittered" then drawGrid (int (Math.Sqrt(float sl.Length)))
        else if sampleMethod = "nrooks" then drawGrid sl.Length
    for (sx, sy) in sl do
        let x = int (float size*sx)
        let y = int (float size*sy)
        for i in [x-(dotSize/2)..x+(dotSize/2)] do
            for j in [y-(dotSize/2)..y+(dotSize/2)] do
                if i >= 0 && j >= 0 && i < size && j < size then img.SetPixel(i, j, Color.Red)
    img.Save(fileName)  

let regular n =
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
    for j in [0..i-1] do
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
    
    for i in [0..n-1] do
        let k = n-1-i
        let shufX = rand.Next(k)
        let _, replY = samples.[shufX]
        let _, currentY = samples.[k]
        samples.[shufX] <- (getJitteredValue shufX n, currentY)
        samples.[k] <- (getJitteredValue k n, replY)
    for i in [0..n-1] do
        let k = n-1-i
        let legalSpots = getLegalSpots k samples
        if not (Array.isEmpty legalSpots) then
            let shufY = legalSpots.[rand.Next(legalSpots.Length)]
            let replX, _ = samples.[shufY]
            let currentX, _ = samples.[k]
            samples.[shufY] <- (currentX, getJitteredValue shufY n)
            samples.[k] <- (replX, getJitteredValue k n)
    List.ofArray samples

let nRooks n =
    let rec placeDiagonals = function
        | 0 -> [(getJitteredValue 0 n, getJitteredValue 0 n)]
        | c -> (getJitteredValue c n, getJitteredValue c n)::(placeDiagonals (c-1))
    let diag = placeDiagonals (n-1)
    shuffleDiagonals diag

(*let stressTest = 
    for i in [0..1920] do
        for j in[0..1080] do
            nRooks 16*)

[<EntryPoint>]
let main argsv =
    let method = argsv.[0]
    let amount = Int32.Parse(argsv.[1])
    let fileName = "sampletest.png"
    match method with
        | "regular" -> drawSamples (regular (float amount)) method fileName
        | "random" -> drawSamples (random amount) method fileName
        | "jittered" -> drawSamples (jittered amount) method fileName
        | "nrooks" -> drawSamples (nRooks amount) method fileName
        | _ -> drawSamples (regular (float amount)) method fileName
    0