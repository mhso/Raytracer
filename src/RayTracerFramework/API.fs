namespace Tracer

open Tracer.Sampling.Sampling
open Tracer.Basics
open Tracer.BaseShape
open Tracer.ImplicitSurfaces.Main
open Transformation

module API = 
  type dummy = unit

  type vector = Vector
  type point = Point
  type colour = Colour
  type material = Material
  type shape = Shape
  type baseShape = BaseShape
  type texture = Texture
  type camera = Camera
  type scene = Scene
  type light = Light
  type ambientLight = AmbientLight
  type transformation = Transformation
  type sampler = SampleGenerator

  let mkRegularSampler (n : int) : sampler = SampleGenerator((fun sm st -> regular sm), n, 1)
  let mkRandomSampler (n : int) (sets : int) : sampler = SampleGenerator(random, n, sets)
  let mkNRooksSampler (n : int) (sets : int) : sampler = SampleGenerator(nRooks, n, sets)
  let mkJitteredSampler (n : int) (sets : int) : sampler = SampleGenerator(jittered, n, sets)
  let mkMultiJitteredSampler (n : int) (sets : int) : sampler = SampleGenerator(multiJittered, n, sets)

  let mkVector (x : float) (y : float) (z : float) : vector = new Vector(x, y, z)
  let mkPoint (x : float) (y : float) (z : float) : point = new Point(x, y, z)
  let fromColor (c : System.Drawing.Color) : colour = new Colour(c)
  let mkColour (r : float) (g : float) (b : float) : colour = new Colour(r, g, b)

  let mkMatteMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) : material = failwith "mkMatteMaterial not implemented"
  let mkPhongMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (exp : int) : material = failwith "mkPhongMaterial not implemented"
  let mkMatteReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cr : colour) (kr : float) : material = failwith "mkMatteReflectiveMaterial not implemented"
  let mkMatteGlossyReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cr : colour) (kr : float) (exps : int) (s : sampler) : material = failwith "mkMatteGlossyReflectiveMaterial not implemented"
  let mkPhongReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (cr : colour) (kr : float) (exps : int) : material = failwith "mkPhongReflectiveMaterial not implemented"
  let mkPhongGlossyReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (cr : colour) (kr : float) (exps : int) (expr : int) (s : sampler) : material = failwith "mkPhongGlossyReflectiveMaterial not implemented"
  let mkEmissive (c : colour) (i : float) : material = new EmissiveMaterial(c, i) :> Material
  let mkTransparent (cf_in : colour) (cf_out : colour) (eta_in : float) (eta_out : float) : material = failwith "mkTransparent not implemented"

  let mkTexture (f : float -> float -> material) : texture = Textures.mkTexture f
  let mkMatTexture (m : material) : texture = Textures.mkMatTexture m

  let mkShape (b : baseShape) (t : texture) : shape = b.toShape t
  let mkSphere (p : point) (r : float) (m : texture) : shape = new SphereShape(p, r, m) :> Shape
  let mkBaseSphere (p : point) (r : float)  : baseShape = new BaseSphere(p, r) :> BaseShape
  let mkBaseRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point) : baseShape = new BaseRectangle(bottomLeft, topLeft, bottomRight) :> BaseShape
  let mkRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point) (t : texture) : shape
    = new Rectangle(bottomLeft, topLeft, bottomRight, t) :> Shape
  let mkTriangle (a:point) (b:point) (c:point) (m : material) : shape = new Triangle(a, b, c, m) :> Shape
  let mkPlane (m : texture) : shape = InfinitePlane(m) :> Shape
  let mkImplicit (s : string) : baseShape = mkImplicit(s)
  let mkPLY (filename : string) (smooth : bool) : baseShape = failwith "mkPoly not implemented"

  let mkHollowCylinder (c : point) (r : float) (h : float) (t : texture) : shape = HollowCylinder(c, r, h, t) :> Shape
  let mkSolidCylinder (c : point) (r : float) (h : float) (t : texture) (top : texture) (bottom : texture) : shape
      = SolidCylinder(c, r, h, t, top, bottom) :> Shape
  let mkDisk (c : point) (r : float) (t : texture) : shape = failwith "mkDisk not implemented"
  let mkBaseDisk (c : point) (r : float) : shape = failwith "mkBaseDisc not implemented"
 
  let mkBox (low : point) (high : point) (front : texture) (back : texture) (top : texture) (bottom : texture) (left : texture) (right : texture) : shape =
      Box(low, high, front, back, top, bottom, left, right) :> Shape
 

  let group (s1 : shape) (s2 : shape) : shape = failwith "group not implemented"
  let union (s1 : shape) (s2 : shape) : shape = failwith "union not implemented"
  let intersection (s1 : shape) (s2 : shape) : shape = failwith "intersection not implemented"
  let subtraction (s1 : shape) (s2 : shape) : shape = failwith "subtraction not implemented"


  let mkLight (p : point) (c : colour) (i : float) : light = new PointLight(c, i, p) :> Light
  let mkDirectionalLight (d : vector) (c : colour) (i : float) : light = new DirectionalLight(c, i, d) :> Light
  let mkAreaLight (bs : baseShape) (m : material) (s : sampler) : light = failwith "mkAreaLight not implemented"
  let mkEnvironmentLight (r : float) (tex : texture) (s : sampler) : light = failwith "mkEnvironmentLight not implemented"
  let mkAmbientLight (c : colour) (i : float) : ambientLight = new AmbientLight(c, i)
  let mkAmbientOccluder (c : colour) (l : float) (lmin : float) (s : sampler) : ambientLight = failwith "mkAmbientOccluder not implemented"

  let mkPinholeCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float) (height : float) (pwidth : int) (pheight : int) (s : sampler) : camera = failwith "mkPinholeCamera not implemented"
  let mkThinLensCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float) (height : float) (pwidth : int) (pheight : int) (radius : float) (fpDistance : float) (pixel : sampler) (lens : sampler) : camera =
    new ThinLensCamera(pos, look, up, zoom, width, height, pwidth, pheight, radius, fpDistance, pixel, lens) :> Camera

  let mkScene (s : shape list) (l : light list) (a : ambientLight)(m : int) : scene = failwith "mkScene not implemented"
  let renderToScreen (sc : scene) (c : camera) : unit = failwith "renderToScreen not implemented"
  let renderToFile (sc : scene) (c : camera) (path : string) : unit = failwith "renderToFile not implemented"

  let translate (x : float) (y : float) (z : float) : transformation = Transformation.translate x y z
  let rotateX (angle : float) : transformation = Transformation.rotateX angle
  let rotateY (angle : float) : transformation = Transformation.rotateY angle
  let rotateZ (angle : float) : transformation = Transformation.rotateZ angle
  let sheareXY (distance : float) : transformation = Transformation.sheare(distance, 0., 0., 0., 0., 0.)
  let sheareXZ (distance : float) : transformation = Transformation.sheare(0., distance, 0., 0., 0., 0.)
  let sheareYX (distance : float) : transformation = Transformation.sheare(0., 0., distance, 0., 0., 0.)
  let sheareYZ (distance : float) : transformation = Transformation.sheare(0., 0., 0., distance, 0., 0.)
  let sheareZX (distance : float) : transformation = Transformation.sheare(0., 0., 0., 0., distance, 0.)
  let sheareZY (distance : float) : transformation = Transformation.sheare(0., 0., 0., 0., 0., distance)
  let scale (x : float) (y : float) (z : float) : transformation = Transformation.scale x y z
  let mirrorX : transformation = failwith "mirrorX not implemented"
  let mirrorY : transformation = failwith "mirrorX not implemented"
  let mirrorZ : transformation = failwith "mirrorX not implemented"
  let mergeTransformations (ts : transformation list) : transformation = Transformation.mergeTransformations ts
  let transform (sh : shape) (tr : transformation) : shape = Transform.transform sh tr
  let transformLight (l : light) (t : transformation) : light = failwith "transformLight not implemented"



  /////////////////////////////////
  // extended API for teams of 7 //
  /////////////////////////////////

  /// Type of acceleration structure
  type Acceleration = KDTree | RegularGrid | BVH

  /// Set which type of acceleration structure to use
  let setAcceleration (accel : Acceleration) : unit = failwith "setAcceleration not implemented"
