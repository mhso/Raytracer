namespace Tracer.Basics.Render

open Tracer.Basics
open Tracer.Basics.Acceleration
open System
open System.Drawing
open System.Windows.Forms
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open Tracer.Basics.Sampling
open System.Runtime.InteropServices
open System.Drawing.Imaging
open System.Resources

type Render(scene : Scene, camera : Camera) =

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
    let up = Vector(0., 1., 0.)
    let ppRendering = false
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
            let ambientColour = hitPoint.Material.AmbientColour(hitPoint, this.Scene.Ambient)
            let totalLightColour = 
                this.Scene.Lights 
                |> List.fold (fun acc light -> 
                    let colour = this.CastRecursively accel ray hitPoint.Shape hitPoint light Colour.Black this.Scene.MaxBounces hitPoint.Material.BounceMethod
                    let occlusion = this.Occlude accel light hitPoint
                    let total = colour + occlusion
                    acc + total) Colour.Black
            ambientColour + totalLightColour
        else
            // If we did not hit, return the background colour
            this.Scene.BackgroundColour

    member this.Occlude accel (light: Light) (hitPoint: HitPoint) = 
        if light :? AmbientOccluder then
            let o = light :?> AmbientOccluder
            let samples = [for i=1 to o.Sampler.SampleCount do yield mapToHemisphere (o.Sampler.Next()) 1.]
            [for (x,y,z) in samples 
                do 
                    let w = hitPoint.Normal.Normalise
                    let v = (up % w).Normalise
                    let u = w % v
                    let spV = Tracer.Basics.Point(x,z,y).OrthonormalTransform(u, v, w)
                    let sp = Tracer.Basics.Point(spV)
                    yield this.CastAmbientOcclusion accel sp o hitPoint ] |> List.average
        else 
            Colour.Black

    member this.CastAmbientOcclusion accel (sp: Tracer.Basics.Point) (o: AmbientOccluder) (hitPoint: HitPoint) = 
        let direction = (hitPoint.Point - sp).Normalise
        let origin = hitPoint.EscapedPoint
        let ray = Ray(origin, direction)
        let (hp:HitPoint) = this.GetFirstShadowHitPoint accel ray
        if hp.DidHit then
            o.MinIntensity * o.Intensity * o.Colour
        else
            o.Intensity * o.Colour

    // Get the first point the ray hits (if it hits, otherwise an empty hit point)
    member this.GetFirstHitPoint accel (ray:Ray) : HitPoint = 

        // Get all hit points for shapes with no bounding boxes
        let pointsThatHit = 
            [for s in nobbshapes do yield s.hitFunction ray]
                |> List.filter (fun hp -> hp.DidHit)
        
        // Add potential hitpoints from Acceleration structure shapes
        let pointsThatHit = let hit = (traverseIAcceleration accel ray bbshapes)
                            if hit.DidHit then hit::pointsThatHit else pointsThatHit

        // Check if the ray hit
        if pointsThatHit.IsEmpty then
            // If not, return an empty hit point
            HitPoint(ray)
        else
            // If the ray hit, then return the first hit point
            pointsThatHit |> List.minBy (fun hp -> hp.Time)


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
        let accel = Acceleration.createAcceleration (shapeArray (idOfScene, bbshapes, None))
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
        let heightInPixel = bitmapData.Height
        let widthInBytes = bitmapData.Width * bytesPrPixel
        
        Parallel.For(0, heightInPixel, fun y ->
        //for y in 0..(heightInPixel-1) do 
            let currentLine = y * bitmapData.Stride
        
            let mutable x = 0
            while (x < widthInBytes) do
                let coordsX = x/bytesPrPixel
                let rays = camera.CreateRays coordsX y
                let cols = Array.map (fun ray -> (this.Cast accel ray)) rays
                let colour = (Array.fold (+) Colour.Black cols)/float cols.Length

                let color = colour.ToColor

                pixel.[currentLine + x] <- (byte)color.B
                pixel.[currentLine + x + 1] <- (byte)color.G
                pixel.[currentLine + x + 2] <- (byte)color.R
                //processed <- processed + 1.0
                //this.CalculateProgress processed total
                x <- x + bytesPrPixel
            ) |> ignore
        Marshal.Copy(pixel, 0, firstPixel, pixel.Length);
        renderedImage.UnlockBits(bitmapData)

        this.PostProcessing
        renderedImage.RotateFlip(RotateFlipType.RotateNoneFlipY)
        renderedImage

    member this.Render =
        // Prepare image
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)

        // Create our timer and Acceleration Structure
        let accel = this.PreProcessing

        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                this.CalculateProgress (float(x*y)) total
                    
                let rays = camera.CreateRays x y
                let colours = Array.map (fun ray -> (this.Cast accel ray)) rays
                let colour = (Array.fold (+) Colour.Black colours)/float colours.Length
                
                renderedImage.SetPixel(x, y, colour.ToColor)

        this.PostProcessing
        renderedImage

    member this.RenderToFile renderMethod filename =
        let image = renderMethod
        this.SaveImage(image, filename)

    member this.RenderToScreen renderMethod =
        let image = renderMethod
        this.ShowImageOnScreen(image)