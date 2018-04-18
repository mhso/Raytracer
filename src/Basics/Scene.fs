﻿namespace Tracer.Basics

open System
open System.Drawing
open System.Diagnostics

type Scene(spheres: Sphere list, camera: Camera, lights: Light list) = 
    let spheres = spheres
    let camera = camera
    let lights = lights
    let backgroundColour = new Colour(1.,1.,1.)
    member this.Spheres = spheres
    member this.Camera = camera
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour   
    member this.Render = 

        // Prepare image
        let renderedImage = new Bitmap(camera.ResX, camera.ResY)
            
        // Field of view
        let n = (camera.Position - camera.Lookat).Normalise
        let v = (n % camera.Up).Normalise
        let hfov = Math.PI/3.5
        let vfov = hfov * float(camera.ResY)/float(camera.ResX)
        let pw = 2.0 * tan(float(hfov/2.0))/float(camera.ResX)
        let ph = 2.0 * tan(float(vfov/2.0))/float(camera.ResY)
            
        // Shoot rays and render image
        for x in 0..camera.ResX-1 do
            for y in 0..camera.ResY-1 do
                let rayOrigin = camera.Position - n + float(x-camera.ResX/2)*pw*camera.Up + float(y-camera.ResY/2)*ph*v
                let rayDirection = (rayOrigin - camera.Position).Normalise
                let ray = new Ray(camera.Position,rayDirection)
                let colour = ray.Cast backgroundColour lights spheres
                renderedImage.SetPixel(x, y, colour.ToColor)

        // Save image
        renderedImage.Save(camera.RenderFilepath)
        
        // Open image
        Process.Start(camera.RenderFilepath)
        