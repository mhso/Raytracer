namespace Tracer.ImplicitSurfaces

module SimpleTracer =

  open System.Drawing
  open Tracer.Basics

  /// This function renders a sphere with the given radvus and colour, centered at the origin,
/// and saves the rendered image into a file of the given name.
  let renderSphere (radius : float)        // The radius of the sphere
                   (colour : Colour)        // The colour of the sphere
                   (position : Point)      // The position of the camera
                   (lookat : Point)        // The point that the camera is looking at
                   (up : Vector)           // The up-vector for the camera
                   (zoom : float)          // Distance to the view plane
                   (width : float)         // Width of the view plane in units
                   (height : float)        // Height of the view plane in units
                   (resX : int)            // The horizontal resolution of the view plane  
                   (resY : int)            // The vertical resolution of the view plane
                   (fileName : string)     // Name of the file to save the rendered image
                   : unit =

    let bm = new System.Drawing.Bitmap (resX, resY)

    let pixelWidth = width / float resX
    let pixelHeight = height / float resY

    // the direction vector for the camera and the view plane
    let camDirection =
      let (lx, ly, lz) = lookat.X, lookat.Y, lookat.Z
      let (px, py, pz) = Point.getCoord position
      mkVector (lx - px) (ly - py) (lz - pz)

    // the view plane
    let w = Vector.normalise camDirection
    let u = Vector.normalise (up % w) // we need the crossProduct
    let v = w % u // this is already normalised, since w and u are
    
    // loops to traverse our bitmap, and paint the individual pixel
    for y in [0 .. resY - 1] do
      for x in [0 .. resX - 1] do
        let px = pixelWidth * ((float x) - ((float resX) / 2.) + 0.5)
        let py = pixelHeight * ((float y) - ((float resY) / 2.) + 0.5)
        // the rayray
        let ray = (position, (px * u) + (py * v) - (zoom * w))
        // fire the cannon!!
        if (hitSphere ray radius) then bm.SetPixel(x, y, colour)
        else bm.SetPixel(x, y, Color.Black)
    // save the bitmap as a png
    bm.Save("output/"+fileName)

  [<EntryPoint>]
  let main argv = 

      // red sphere, with the camera in front of it
      renderSphere 2.0 Color.Red (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 512 512 "front.png"
      // larger golden sphere, with the camera in front of it
      renderSphere 3.0 Color.Gold (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 512 512 "big.png"
      // green sphere, with the camera in front of it
      renderSphere 2.0 Color.Green (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 2.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 512 512 "up.png"
      // blue sphere, with the camera inside of it
      renderSphere 2.0 Color.Blue (mkPoint 0.0 0.0 0.0) (mkPoint 0.0 0.0 -4.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 512 512 "inside.png"
      // yellow sphere, with the camera behind it (pointing in the other direction)
      renderSphere 2.0 Color.Yellow (mkPoint 0.0 0.0 -4.0) (mkPoint 0.0 0.0 -8.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 512 512 "behind.png"

      0