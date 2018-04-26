namespace Tracer.Basics

open System
open System.Drawing
open System.Diagnostics

type Scene(spheres: Shape list, camera: Camera, lights: Light list) = 
    let spheres = spheres
    let camera = camera
    let lights = lights
    let backgroundColour = new Colour(0., 0., 0.)
    member this.Spheres = spheres
    member this.Camera = camera
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour   
    member this.Render = 

        // Prepare image
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)
            
        // Field of view
        let n = (camera.Position - camera.Lookat).Normalise
        let u = (camera.Up % n)
        let v = (n % u)
        let hfov = Math.PI/3.5
        let vfov = hfov * float(camera.ResY)/float(camera.ResX)
        let pw = 2.0 * tan(float(hfov/2.0))/float(camera.ResX)
        let ph = 2.0 * tan(float(vfov/2.0))/float(camera.ResY)
        let vpc = camera.Position - n    

        // Shoot rays and render image
        let total = float (camera.ResX * camera.ResY)
        let mutable currPct = 0
        let loadingSymbols = [|"|"; "/"; "―"; "\\"; "|"; "/"; "―"; "\\"|]
        let mutable loadingIndex = 0
        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                camera.Cast x y
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
                renderedImage.SetPixel(x, y, colour.ToColor)

        // Save image
        renderedImage.Save(camera.RenderFilepath)
        
        // Open image
        Process.Start(camera.RenderFilepath)
        