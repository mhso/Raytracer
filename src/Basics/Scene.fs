namespace Tracer.Basics

open System
open System.Drawing
open System.Diagnostics
open System.Threading.Tasks
open System.Threading
open Acceleration

type Scene(shapes: Shape list, lights: Light list, ambient : AmbientLight, maxBounces : int) = 

    let backgroundColour = new Colour(0., 0., 0.)
    member this.Shapes = shapes
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour 
    member this.MaxBounces = maxBounces