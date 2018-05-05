namespace Tracer.Basics

open System
open System.Drawing
open System.Diagnostics
open System.Threading.Tasks

type Scene(shapes: Shape[], camera: Camera, lights: Light list, ambient : AmbientLight, maxBounces) = 

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
    member this.MaxBounces = maxBounces
            
    member this.Cast ray =
        // Get the hitpoint
        let (shape, (hitPoint: HitPoint)) = this.GetFirstHitPoint ray

        // Check if we hit
        if hitPoint.DidHit then
            // Sum the light colors for that hitpoint
            let normal = hitPoint.Normal
            lights 
                |> List.fold (fun accColour light -> 
                    let shadowColour = this.CastShadow hitPoint light
                    let colour = this.CastRecursively ray shape hitPoint light Colour.Black this.MaxBounces hitPoint.Material.BounceMethod
                    accColour + (colour - shadowColour)) (new Colour(0.,0.,0.))
        else
            // If we did not hit, return the background colour
            this.BackgroundColour

    // Get the first point the ray hits (if it hits, otherwise an empty hit point)
    member this.GetFirstHitPoint (ray:Ray) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction ray )]
                |> List.filter (fun (_,hp:HitPoint) -> hp.DidHit)
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape.None, HitPoint(ray))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

    member this.GetFirstShadowHitPoint (ray:Ray) = 
        
        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction ray )]
                |> List.filter (fun (_,hp:HitPoint) -> hp.DidHit)
                |> List.filter (fun (_,hp:HitPoint) -> not (hp.Material :? EmissiveMaterial)) // Filter out emisive materials
        
        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape.None, HitPoint(ray))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)


    member this.GetFirstHitPointExcept (ray: Ray) (except: Shape) = 

        // Get all hit points
        let pointsThatHit = 
            [for s in shapes do yield (s, s.hitFunction ray)]
                |> List.filter (fun (_,hp) -> hp.DidHit)
                |> List.filter (fun (shape,_) -> 
                    let eq = Object.ReferenceEquals(shape, except)
                    not eq)

        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            (Shape.None, new HitPoint(ray))
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun (_,hp) -> hp.Time)

    // Returns the average shadow for a hitpoint and a light source
    member this.CastShadow (hitPoint: HitPoint) (light: Light) = 
        if light :? AmbientLight 
            then Colour.Black
        else
            let shadowRays = light.GetShadowRay hitPoint
            let isShadow ray = 
                let (_, hp) = (this.GetFirstShadowHitPoint ray)
                if hp.DidHit then
                        Colour.White 
                    else 
                        Colour.Black
            
            if shadowRays.Length = 0 then 
                Colour.Black
            else
                let totalShadow = Array.fold (fun acc ray -> acc + isShadow ray) Colour.Black shadowRays
                (totalShadow / float(shadowRays.Length))

    // Will cast a ray recursively
    member this.CastRecursively 
        (incomingRay: Ray) (shape: Shape) (hitPoint: HitPoint) (light: Light) (acc: Colour) (bounces: int) (reflectionFunction: HitPoint -> Ray[]) =
        if bounces = 0 || not hitPoint.Material.IsRecursive then
            acc + hitPoint.Material.PreBounce shape hitPoint light
        else
            let outRay = reflectionFunction hitPoint
            let baseColour = acc + hitPoint.Material.PreBounce shape hitPoint light
            let mutable outColour = Colour.Black
            for i = 0 to outRay.Length-1 do
                outColour <- outColour + 
                    let (outShape, outHitPoint) = this.GetFirstHitPointExcept outRay.[i] shape
                    if outHitPoint.DidHit then
                        this.CastRecursively outRay.[i] outShape outHitPoint light baseColour (bounces - 1) reflectionFunction
                    else
                        Colour.Black
            baseColour + (outColour / float(outRay.Length))

    member this.CalculateProgress current total =
        let pct = int((current/total) * 100.0)
        // Progress bar!!!
        if pct > currentPct then 
            if loadingIndex = loadingSymbols.Length then loadingIndex <- 0
            currentPct <- pct

            let dots = String.replicate (currentPct/2 + 1) "█"
            let white = String.replicate (49-(currentPct/2)) "░"

            Console.Write("\r                               {0}", loadingSymbols.[loadingIndex] + " |" + dots + white + "| " + string (pct+1) + "%")
            loadingIndex <- loadingIndex + 1

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
            let cols = List.map (fun ray -> (this.Cast ray)) rays
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
                let colours = List.map (fun ray -> (this.Cast ray)) rays
                let colour = (List.fold (+) Colour.Black colours)/float colours.Length

                renderedImage.SetPixel(x, y, colour.ToColor)

        this.EndRender renderedImage