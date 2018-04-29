namespace Tracer.Basics

open System
open System.Drawing
open System.Diagnostics

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
        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                let pct = int((float (x*y)/total) * 100.0)

                // Progress bar!!!
                if pct > currPct then 
                    Console.Clear()
                    printf "%s" ("Rendering: " + loadingSymbols.[loadingIndex] + " |")
                    currPct <- pct
                    let dots = String.replicate (currPct/2) "█"
                    let white = String.replicate (50-(currPct/2)) "░"
                    loadingIndex <- loadingIndex + 1
                    if loadingIndex = loadingSymbols.Length then loadingIndex <- 0
                    printf "%s" (dots + white)
                    printf "%s"  ("| " + string pct + "%")
                
                let ray = camera.CreateRay x y
                let colour = camera.Cast ray backgroundColour shapes lights
                renderedImage.SetPixel(x, y, colour.ToColor)

        
        // Save image
        renderedImage.Save(camera.RenderFilepath)
        
        // Open image
        Process.Start(camera.RenderFilepath)

    
        