namespace Tracer.Basics

open System
open System.Drawing
open System.Diagnostics
open System.Threading.Tasks

type Scene(shapes: Shape list, camera: Camera, lights: Light list) = 

    let backgroundColour = new Colour(0., 0., 0.)
    member this.Spheres = shapes
    member this.Camera = camera
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour   
            
    member this.Render = 

        // Prepare image
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)

        // Shoot rays and render image
        let total = float (camera.ResX * camera.ResY)
        let mutable currPct = 0

        let loadingSymbols = [|"|"; "/"; "-"; @"\"; "|"; "/"; "-"; @"\"|]
        let mutable loadingIndex = 0
        Console.WriteLine("
        



                           ██▀███ ▓█████ ███▄    █▓█████▄▓█████ ██▀███  ██▓███▄    █  ▄████ 
                           ▓██ ▒ ██▓█   ▀ ██ ▀█   █▒██▀ ██▓█   ▀▓██ ▒ ██▓██▒██ ▀█   █ ██▒ ▀█▒
                           ▓██ ░▄█ ▒███  ▓██  ▀█ ██░██   █▒███  ▓██ ░▄█ ▒██▓██  ▀█ ██▒██░▄▄▄░
                           ▒██▀▀█▄ ▒▓█  ▄▓██▒  ▐▌██░▓█▄   ▒▓█  ▄▒██▀▀█▄ ░██▓██▒  ▐▌██░▓█  ██▓
                           ░██▓ ▒██░▒████▒██░   ▓██░▒████▓░▒████░██▓ ▒██░██▒██░   ▓██░▒▓███▀▒
                           ░ ▒▓ ░▒▓░░ ▒░ ░ ▒░   ▒ ▒ ▒▒▓  ▒░░ ▒░ ░ ▒▓ ░▒▓░▓ ░ ▒░   ▒ ▒ ░▒   ▒ 
                               ░▒ ░ ▒░░ ░  ░ ░░   ░ ▒░░ ▒  ▒ ░ ░  ░ ░▒ ░ ▒░▒ ░ ░░   ░ ▒░ ░   ░ 
                               ░░   ░   ░     ░   ░ ░ ░ ░  ░   ░    ░░   ░ ▒ ░  ░   ░ ░░ ░   ░ 
                               ░       ░  ░        ░   ░      ░  ░  ░     ░          ░      ░ 
                                                       ░                                        
                                                                                                ")
        
        let timer = System.Diagnostics.Stopwatch.StartNew()

        let pos = [for y in 0 .. camera.ResY - 1 do
                    for x in 0 .. camera.ResX - 1 do yield (x,y)]

        let bmColourArray = Array2D.zeroCreate camera.ResY camera.ResX

        Parallel.ForEach (pos, fun (x,y) -> 
            let rays = camera.CreateRays x y
            let cols = List.map (fun ray -> (camera.Cast ray backgroundColour shapes lights)) rays
            let colour = (List.fold (+) Colour.Black cols)/float cols.Length
            bmColourArray.[y,x] <- colour) |> ignore

        for y in 0 .. camera.ResY - 1 do
          for x in 0 .. camera.ResX - 1 do
            renderedImage.SetPixel(x, y, bmColourArray.[y,x].ToColor)

        (*
        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                let pct = int((float (x*y)/total) * 100.0)

                // Progress bar!!!
                if pct > currPct then 
                    currPct <- pct

                    let dots = String.replicate (currPct/2 + 1) "█"
                    let white = String.replicate (49-(currPct/2)) "░"
                    loadingIndex <- loadingIndex + 1
                    if loadingIndex = loadingSymbols.Length then loadingIndex <- 0

                    Console.Write("\r                                {0}", loadingSymbols.[loadingIndex] + " |" + dots + white + "| " + string (pct+1) + "%");
                    
                let rays = camera.CreateRays x y
                let colours = List.map (fun ray -> (camera.Cast ray backgroundColour shapes lights)) rays
                let colour = (List.fold (+) Colour.Black colours)/float colours.Length
                renderedImage.SetPixel(x, y, colour.ToColor)*)
                
        // Save image
        renderedImage.Save(camera.RenderFilepath)
        
        // Open image
        Process.Start(camera.RenderFilepath) |> ignore

        // Printing how much time was spent rendering
        timer.Stop()
        printfn "Seconds: %f" timer.Elapsed.TotalSeconds

        System.Console.ReadKey () |> ignore