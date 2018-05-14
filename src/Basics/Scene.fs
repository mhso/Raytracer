namespace Tracer.Basics

type Scene(shapes: Shape list, lights: Light list, ambient : AmbientLight, maxBounces : int) = 

    let backgroundColour = new Colour(0., 0., 0.)
    member this.Shapes = shapes
    member this.Ambient = ambient
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour 
    member this.MaxBounces = maxBounces