namespace Tracer.Basics

open System.Drawing
open System

type Camera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point, up: Vector, zoom: float, width: float, height: float, resX: int, resY: int) =
    let position = position
    let lookat = lookat
    let up = up
    let zoom = zoom
    let width = width
    let height = height
    let resX = resX
    let resY = resY
    member this.Position = position
    member this.Lookat = lookat
    member this.Up = up
    member this.Zoom = zoom
    member this.Width = width
    member this.Height = height
    member this.ResX = resX
    member this.ResY = resY
    member this.RenderFilepath = "background.bmp"
    
        
        

