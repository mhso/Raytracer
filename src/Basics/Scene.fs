namespace Tracer.Basics

open System
open System.Drawing
open System.Diagnostics
open System.Threading.Tasks

type Scene(shapes: Shape[], camera: Camera, lights: Light list) = 

    let backgroundColour = new Colour(0., 0., 0.)
    let total = float (camera.ResX * camera.ResY)
    let loadingSymbols = [|"|"; "/"; "-"; @"\"; "|"; "/"; "-"; @"\"|]
    let timer = new System.Diagnostics.Stopwatch()
    let mutable currentPct = 0
    let mutable loadingIndex = 0
    member this.Spheres = shapes
    member this.Camera = camera
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour 
            
    member this.CalculateProgress current total =
        let pct = int((current/total) * 100.0)
        // Progress bar!!!
        if pct > currentPct then 
            currentPct <- pct

            let dots = String.replicate (currentPct/2 + 1) "█"
            let white = String.replicate (49-(currentPct/2)) "░"
            loadingIndex <- loadingIndex + 1
            if loadingIndex = loadingSymbols.Length then loadingIndex <- 0

            Console.Write("\r                                {0}", loadingSymbols.[loadingIndex] + " |" + dots + white + "| " + string (pct+1) + "%")

    member this.StartRender =
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
        timer.Start()

    member this.EndRender (renderedImage:Bitmap) =
        // Save image
        renderedImage.Save(camera.RenderFilepath)
        
        // Open image
        Process.Start(camera.RenderFilepath) |> ignore

        // Printing how much time was spent rendering
        timer.Stop()
        printfn ""
        printfn ""
        printfn "                                                     Seconds: %f" timer.Elapsed.TotalSeconds

        System.Console.ReadKey () |> ignore

    member this.RenderParallel = 
        // Prepare image
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)

        this.StartRender
        
        let mutable processed = 0.0

        let pos = [for y in 0 .. camera.ResY - 1 do
                    for x in 0 .. camera.ResX - 1 do yield (x,y)]

        let bmColourArray = Array2D.zeroCreate camera.ResY camera.ResX

        // Shoot rays and save the resulting colors, using parallel computations.
        Parallel.ForEach (pos, fun (x,y) -> 
            let rays = camera.CreateRays x y
            let cols = List.map (fun ray -> (camera.Cast ray backgroundColour shapes lights)) rays
            let colour = (List.fold (+) Colour.Black cols)/float cols.Length
            bmColourArray.[y,x] <- colour
            
            processed <- processed + 1.0
            this.CalculateProgress processed total
            ) |> ignore

        // Apply the colors to the image.
        for y in 0 .. camera.ResY - 1 do
          for x in 0 .. camera.ResX - 1 do
            renderedImage.SetPixel(x, y, bmColourArray.[y,x].ToColor)

        this.EndRender renderedImage

    member this.Render =
        // Prepare image
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)

        this.StartRender

        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                this.CalculateProgress (float(x*y)) total
                    
                let rays = camera.CreateRays x y
                let colours = List.map (fun ray -> (camera.Cast ray backgroundColour shapes lights)) rays
                let colour = (List.fold (+) Colour.Black colours)/float colours.Length

                renderedImage.SetPixel(x, y, colour.ToColor)

        this.EndRender renderedImage