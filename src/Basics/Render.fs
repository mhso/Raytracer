namespace Tracer.Basics.Render

open Tracer.Basics
open Tracer.Basics.Acceleration
open System
open System.Drawing
open System.Windows.Forms
open System.Threading.Tasks
open Tracer.Basics.Sampling
open System.Runtime.InteropServices
open System.Drawing.Imaging

type Render(scene : Scene, camera : Camera) =
    let accelTiming = true
    let travTimer = new System.Diagnostics.Stopwatch()
    let renderTimer = new System.Diagnostics.Stopwatch()

    // Pre-rendering
    let rec filtershapes (nobb: Shape list) (bb : Shape list) = function
      | []            -> nobb, List.toArray bb
      | (c:Shape)::cr -> 
          try 
            c.getBoundingBox() |> ignore
            filtershapes nobb (c::bb) cr
          with 
            | _ -> filtershapes (c::nobb) bb cr                   
    let (nobbshapes, bbshapes) = filtershapes [] [] scene.Shapes

    // Printing render status
    let total = float (camera.ResX * camera.ResY)
    let loadingSymbols = [|"|"; "/"; "-"; @"\"; "|"; "/"; "-"; @"\"|]
    let timer = new System.Diagnostics.Stopwatch()

    let ppRendering = true
    let mutable currentPct = 0
    let mutable loadingIndex = 0
    let randomStrings = [|"                                                      Traversing..."; 
                          "                                                     Shooting Rays..."; 
                          "                                              Applying Ambient Occlusion..."; 
                          "                                                       Sampling..."; 
                          "                                                 Transforming Bunnies..."; 
                          "                                                 Stretching Triangles..."; 
                          "                                                   Spawning Spheres..."; 
                          "                                              Initializing Stackoverflow..."; 
                          "                                                Creating Infinity Loops..."; 
                          "                                               Making Surfaces Implicit..."; 
                          "                                       Making Infinite Planes infinity + 1 long...";
                          "                                             Deleting Random System File...";
                          "                                          RayTracer.exe Has Stopped Working..."|]
    let getRandomString () =  
      let random = System.Random()  
      randomStrings.[random.Next(randomStrings.Length)]

    let idOfScene = Acceleration.listOfKDTree.Length + 1
    member this.Camera = camera
    member this.Scene = scene
    member this.Shapes = List.toArray scene.Shapes

    member this.Cast accel ray =
        // Get the hitpoint
        let hitPoint: HitPoint = this.GetFirstHitPoint accel ray

        // Check if we hit
        if hitPoint.DidHit then
            // Sum the light colors for that hitpoint
            let ambientColour = this.CastAmbientColour accel hitPoint
            let totalLightColour = 
                this.Scene.Lights 
                |> List.fold (fun acc light -> 
                    let colour = this.CastRecursively accel ray hitPoint.Shape hitPoint light Colour.Black this.Scene.MaxBounces hitPoint.Material.BounceMethod
                    acc + colour) Colour.Black
            ambientColour + totalLightColour
        else
            // If we did not hit, return the background colour
            this.Scene.BackgroundColour

    member this.CastAmbientColour accel hitPoint = 
        hitPoint.Material.AmbientColour(hitPoint, this.Scene.Ambient) *
            match this.Scene.Ambient with
                | :? AmbientOccluder as occluder -> this.Occlude accel occluder hitPoint
                | _ -> Colour.White

    member this.Occlude accel (occluder: AmbientOccluder) (hitPoint: HitPoint) = 
        let sampler = occluder.Sampler
        let transOrthoCoord (x,y,z) = 
            
            let sp = Tracer.Basics.Point(x/2.,y/2.,z)

            // Reflected ray direction
            let m = hitPoint.Normal

            // Transform orthonormal frame of sample point
            let up = new Vector(0., 1., 0.)
            let w = hitPoint.Normal
            let v = (up % w).Normalise
            let u = w % v
            sp.OrthonormalTransform(u, v, w)

        let total = [for i=0 to sampler.SampleCount do yield transOrthoCoord (mapToHemisphere (sampler.Next()) 0.)]
                    |> List.fold (fun acc ad -> acc + this.CastAmbientOcclusion accel ad occluder hitPoint) Colour.Black 

        total / sampler.SampleCount
        

    member this.CastAmbientOcclusion accel (sp: Vector) (occluder: AmbientOccluder) (hitPoint: HitPoint) = 
        let ray = Ray(hitPoint.EscapedPoint, sp)
        let rayHit = this.GetFirstHitPoint accel ray
        if rayHit.DidHit then
            occluder.MinIntensityColour
        else
            occluder.Colour       

    // Get the first point the ray hits (if it hits, otherwise an empty hit point)
    member this.GetFirstHitPoint accel (ray:Ray) : HitPoint = 
      let rec findClosestHit (h:HitPoint) t' = function
      | []    -> 
          let hit = traverseIAcceleration accel ray bbshapes
          if hit.DidHit && hit.Time < t' then hit
          else h
      | (s:Shape)::sl -> 
          let hit = s.hitFunction ray
          if hit.DidHit && hit.Time < t' then findClosestHit hit hit.Time sl
          else findClosestHit h t' sl
      findClosestHit (HitPoint(ray)) infinity nobbshapes

    member this.GetFirstShadowHitPoint accel (ray:Ray) : HitPoint = 
        let hit = this.GetFirstHitPoint accel ray
        if hit.Material :? EmissiveMaterial then HitPoint(ray) // no shadow if we have direct rout to emissive material
        else hit

    // Returns the average shadow for a hitpoint and a light source
    member this.CastShadow accel (hitPoint: HitPoint) (light: Light) : Colour = 
        if light :? AmbientLight || hitPoint.Material :? TransparentMaterial
            then Colour.Black
        else
            let shadowRays = light.GetShadowRay hitPoint
            
            let maxTime =
                match light with
                | :? PointLight as p -> p.Position.Distance(hitPoint.Point).Magnitude
                | _ -> 2147483647.

            let isShadow ray = 
                let hp = (this.GetFirstShadowHitPoint accel ray)
                if hp.DidHit && hp.Time < maxTime then
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
        (accel: IAcceleration) (incomingRay: Ray) (shape: Shape) (hitPoint: HitPoint) (light: Light) (acc: Colour) (bounces: int) 
        (reflectionFunction: HitPoint -> Ray[]) : Colour =

        let shadowColour = this.CastShadow accel hitPoint light
        if bounces = 0 || not hitPoint.Material.IsRecursive then
            acc + (hitPoint.Material.PreBounce(shape, hitPoint, light, this.Scene.Ambient) - shadowColour)
        else
            let outRay = reflectionFunction hitPoint
            let baseColour = acc + (hitPoint.Material.PreBounce(shape, hitPoint, light, this.Scene.Ambient) - shadowColour)
            let mutable outColour = Colour.Black
            for i = 0 to outRay.Length-1 do
                 
                    let outHitPoint = this.GetFirstHitPoint accel outRay.[i]
                    if outHitPoint.DidHit then
                        let recursiveColour = this.CastRecursively accel outRay.[i] outHitPoint.Shape outHitPoint light baseColour (bounces - 1) reflectionFunction
                        outColour <- outColour + hitPoint.Material.ReflectionFactor(hitPoint, outRay.[i]) * recursiveColour
                    else
                        outColour <- outColour + this.Scene.BackgroundColour
            baseColour + (outColour)

    member this.CalculateProgress current total =
        let pct = int((current/total) * 100.0)
        // Progress bar!!!
        if pct > currentPct then 
            if loadingIndex = loadingSymbols.Length then loadingIndex <- 0
            currentPct <- pct

            let dots = String.replicate (currentPct/2 + 1) "█"
            let white = String.replicate (50-(currentPct/2)) "░"

            timer.Stop()
            let msSpent = float timer.ElapsedMilliseconds
            timer.Start()
            let msRemaining = 
              if currentPct <> 100 then ((100. - float currentPct) / float currentPct) * float msSpent
              else 0.0000
            let secondsRemaining = System.Math.Round (msRemaining * 0.001, 4)
            Console.SetCursorPosition (0, Console.CursorTop - 2)
            Console.Write("                               {0}", loadingSymbols.[loadingIndex] + " |" + dots + white + "| " + string pct + "%")
            printf ("\n\n                                             Time remaining: %.4f") secondsRemaining
            printf " seconds                   "
            loadingIndex <- loadingIndex + 1

    member this.PreProcessing =
        // Start timer for acceleration create measurement
        if accelTiming then 
            travTimer.Start() 
            printfn "# Acceleration create timing start"

        let accel = Acceleration.createAcceleration (shapeArray (idOfScene, bbshapes, None))

        // Stop timer for acceleration create measurement and print elapsed time
        if accelTiming then
            travTimer.Stop()
            printfn "## Acceleration create in %f seconds" travTimer.Elapsed.TotalSeconds

        if ppRendering then
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
          printfn "%s" (getRandomString())
          //Console.WriteLine("                                                   Building Acceleration Structure..")
        else ()
        
        //let kdTimer = Stopwatch.StartNew()
        //kdTimer.Stop()
        
        //if ppRendering then
         // Console.WriteLine("                                                   ...Done in " + string kdTimer.ElapsedMilliseconds + " ms.\n\n")
          //Console.WriteLine()
        //else ()

        accel

    member this.PostProcessing =
        timer.Stop()
        // Printing how much time was spent rendering
        if ppRendering then
          printfn ""
          printfn ""
          printfn "                                            Rendering Time: %f Seconds\n\n" timer.Elapsed.TotalSeconds

    member this.ShowImageOnScreen (renderedImage:Bitmap) =
        let window = new Form(ClientSize=Size(renderedImage.Width, renderedImage.Height), StartPosition=FormStartPosition.CenterScreen)
        window.Paint.Add(fun draw -> draw.Graphics.DrawImage(renderedImage, Point(0, 0)))
        Application.Run(window)

    member this.SaveImage (renderedImage:Bitmap, filepath) =
        // Save image
        renderedImage.Save(filepath)
        
        // Open image
        //Process.Start(filepath) |> ignore

    member this.RenderParallel = 
        if accelTiming then 
            renderTimer.Start()
            printfn "# Acceleration RenderParallel timing start"

        // Create our timer and Acceleration Structure
        let accel = this.PreProcessing
        timer.Start()

        // Prepare image
        let renderedImage = (new Bitmap(camera.ResX, camera.ResY))
        use g = Graphics.FromImage(renderedImage)
        use brush = new SolidBrush(Color.Black)
        g.FillRectangle(brush, 0,0,camera.ResX,camera.ResY)
        
        //ref: http://csharpexamples.com/fast-image-processing-c/
        let bitmapData = renderedImage.LockBits(new Rectangle(0, 0, renderedImage.Width, renderedImage.Height), ImageLockMode.ReadWrite, renderedImage.PixelFormat)
        let bytesPrPixel = Bitmap.GetPixelFormatSize(renderedImage.PixelFormat) / 8
        let byteCount = bitmapData.Stride * renderedImage.Height
        let pixel : byte[] = Array.zeroCreate(byteCount)
        let firstPixel = bitmapData.Scan0
        Marshal.Copy(firstPixel, pixel, 0, pixel.Length)
        
        // Start timer for acceleration traverse measurement
        if accelTiming then 
            travTimer.Start()
            printfn "# Acceleration traverese timing start"

        Parallel.For(0, bitmapData.Height * bitmapData.Width, fun xy ->
            let y = xy / bitmapData.Width
            let x = (xy % bitmapData.Width) * bytesPrPixel

            let currentLine = y * bitmapData.Stride
        
            let coordsX = x/bytesPrPixel
            let rays = camera.CreateRays coordsX y
            let cols = Array.map (fun ray -> (this.Cast accel ray)) rays
            let colour = (Array.fold (+) Colour.Black cols)/float cols.Length

            let color = colour.ToColor

            pixel.[currentLine + x] <- (byte)color.B
            pixel.[currentLine + x + 1] <- (byte)color.G
            pixel.[currentLine + x + 2] <- (byte)color.R

            ) |> ignore

        Marshal.Copy(pixel, 0, firstPixel, pixel.Length);
        renderedImage.UnlockBits(bitmapData)

        // Stop timer for acceleration traverse measurement and print elapsed time
        if accelTiming then
            travTimer.Stop()
            printfn "## Acceleration traverse in %f seconds" travTimer.Elapsed.TotalSeconds

        this.PostProcessing
        renderedImage.RotateFlip(RotateFlipType.RotateNoneFlipY)

        // Stop timer for render measurement and print elapsed time
        if accelTiming then
            renderTimer.Stop()
            printfn "## RenderParallel in %f seconds" renderTimer.Elapsed.TotalSeconds
        renderedImage

    member this.Render =
        if accelTiming then 
            renderTimer.Start()
            printfn "# Acceleration render timing start"

        // Prepare image
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)

        // Create our timer and Acceleration Structure
        let accel = this.PreProcessing

        // Start timer for acceleration traverse measurement
        if accelTiming then 
            travTimer.Start()
            printfn "# Acceleration traverse timing start"

        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                if ppRendering then this.CalculateProgress (float(x*y)) total
                    
                let rays = camera.CreateRays x y
                let colours = Array.map (fun ray -> (this.Cast accel ray)) rays
                let colour = (Array.fold (+) Colour.Black colours)/float colours.Length
                
                renderedImage.SetPixel(x, y, colour.ToColor)
        
        // Stop timer for acceleration traverse measurement and print elapsed time
        if accelTiming then
            travTimer.Stop()
            printfn "## Acceleration traverse in %f seconds" travTimer.Elapsed.TotalSeconds

        this.PostProcessing
        // Stop timer for render measurement and print elapsed time
        if accelTiming then
            renderTimer.Stop()
            printfn "## Render in %f seconds" renderTimer.Elapsed.TotalSeconds
        renderedImage.RotateFlip(RotateFlipType.RotateNoneFlipY)

    member this.RenderToFile renderMethod filename =
        let image = renderMethod
        this.SaveImage(image, filename)

    member this.RenderToScreen renderMethod =
        let image = renderMethod
        this.ShowImageOnScreen(image)