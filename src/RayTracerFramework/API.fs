namespace Tracer

module API = 
  type dummy = unit

  type vector = dummy
  type point = dummy
  type colour = dummy
  type material = dummy
  type shape = dummy
  type baseShape = dummy
  type texture = dummy
  type camera = dummy
  type scene = dummy
  type light = dummy
  type ambientLight = dummy
  type transformation = dummy
  type sampler = dummy

  let mkRegularSampler (n : int) : sampler = failwith "mkRegularSampler not implemented"
  let mkRandomSampler (n : int) (sets : int) : sampler = failwith "mkRandomSampler not implemented"
  let mkNRooksSampler (n : int) (sets : int) : sampler = failwith "mkNRooksSampler not implemented"
  let mkJitteredSampler (n : int) (sets : int) : sampler = failwith "mkJitteredSampler not implemented"
  let mkMultiJitteredSampler (n : int) (sets : int) : sampler = failwith "mkMultiJitteredSampler not implemented"

  let mkVector (x : float) (y : float) (z : float) : vector = failwith "mkVector not implemented"
  let mkPoint (x : float) (y : float) (z : float) : point = failwith "mkPoint not implemented"
  let fromColor (c : System.Drawing.Color) : colour = failwith "fromColor not implemented"
  let mkColour (r : float) (g : float) (b : float) : colour = failwith "mkColour not implemented"

  let mkMatteMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) : material = failwith "mkMatteMaterial not implemented"
  let mkPhongMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (exp : int) : material = failwith "mkPhongMaterial not implemented"
  let mkMatteReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cr : colour) (kr : float) : material = failwith "mkMatteReflectiveMaterial not implemented"
  let mkMatteGlossyReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cr : colour) (kr : float) (exps : int) (s : sampler) : material = failwith "mkMatteGlossyReflectiveMaterial not implemented"
  let mkPhongReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (cr : colour) (kr : float) (exps : int) : material = failwith "mkPhongReflectiveMaterial not implemented"
  let mkPhongGlossyReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (cr : colour) (kr : float) (exps : int) (expr : int) (s : sampler) : material = failwith "mkPhongGlossyReflectiveMaterial not implemented"
  let mkEmissive (c : colour) (i : float) : material = failwith "mkEmissive not implemented"
  let mkTransparent (cf_in : colour) (cf_out : colour) (eta_in : float) (eta_out : float) : material = failwith "mkTransparent not implemented"

  let mkTexture (f : float -> float -> material) : texture = failwith "mkTexture not implemented"
  let mkMatTexture (m : material) : texture = failwith "mkMatTexture not implemented"

  let mkShape (b : baseShape) (t : texture) : shape = failwith "mkShape not implemented"
  let mkSphere (p : point) (r : float) (m : texture) : shape = failwith "mkSphere not implemented"
  let mkBaseSphere (p : point) (r : float)  : baseShape = failwith "mkBaseSphere not implemented"
  let mkBaseRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point) : baseShape = failwith "mkBaseRectangle not implemented"
  let mkRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point) (t : texture) : shape
    = failwith "mkRectangle not implemented"
  let mkTriangle (a:point) (b:point) (c:point) (m : material) : shape = failwith "mkTriangle not implemented"
  let mkPlane (m : texture) : shape = failwith "mkPlane not implemented"
  let mkImplicit (s : string) : baseShape = failwith "mkImplicit not implemented"
  let mkPLY (filename : string) (smooth : bool) : baseShape = failwith "mkPLY not implemented"

  let mkHollowCylinder (c : point) (r : float) (h : float) (t : texture) : shape = failwith "mkHollowCylinder not implemented"
  let mkSolidCylinder (c : point) (r : float) (h : float) (t : texture) (top : texture) (bottom : texture) : shape
      = failwith "mkSolidCylinder not implemented"
  let mkDisk (c : point) (r : float) (t : texture) : shape = failwith "mkDisk not implemented"
  let mkBaseDisk (c : point) (r : float) : shape = failwith "mkBaseDisc not implemented"
 
  let mkBox (low : point) (high : point) (front : texture) (back : texture) (top : texture) (bottom : texture) (left : texture) (right : texture) : shape
      = failwith "mkBox not implemented"
 

  let group (s1 : shape) (s2 : shape) : shape = failwith "group not implemented"
  let union (s1 : shape) (s2 : shape) : shape = failwith "union not implemented"
  let intersection (s1 : shape) (s2 : shape) : shape = failwith "intersection not implemented"
  let subtraction (s1 : shape) (s2 : shape) : shape = failwith "subtraction not implemented"


  let mkLight (p : point) (c : colour) (i : float) : light = failwith "mkLight not implemented"
  let mkDirectionalLight (d : vector) (c : colour) (i : float) : light = failwith "mkDirectionalLight not implemented"
  let mkAreaLight (bs : baseShape) (m : material) (s : sampler) : light = failwith "mkAreaLight not implemented"
  let mkEnvironmentLight (r : float) (tex : texture) (s : sampler) : light = failwith "mkEnvironmentLight not implemented"
  let mkAmbientLight (c : colour) (i : float) : ambientLight = failwith "mkAmbientLight not implemented"
  let mkAmbientOccluder (c : colour) (l : float) (lmin : float) (s : sampler) : ambientLight = failwith "mkAmbientOccluder not implemented"

  let mkPinholeCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float) (height : float) (pwidth : int) (pheight : int) (s : sampler) : camera = failwith "mkPinholeCamera not implemented"
  let mkThinLensCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float) (height : float) (pwidth : int) (pheight : int) (radius : float) (fpDistance : float) (pixel : sampler) (lens : sampler) : camera = failwith "mkThinLensCamera not implemented"

  let mkScene (s : shape list) (l : light list) (a : ambientLight)(m : int) : scene = failwith "mkScene not implemented"
  let renderToScreen (sc : scene) (c : camera) : unit = failwith "renderToScreen not implemented"
  let renderToFile (sc : scene) (c : camera) (path : string) : unit = failwith "renderToFile not implemented"

  let translate (x : float) (y : float) (z : float) : transformation = failwith "translate not implemented"
  let rotateX (angle : float) : transformation = failwith "rotateX not implemented"
  let rotateY (angle : float) : transformation = failwith "rotateY not implemented"
  let rotateZ (angle : float) : transformation = failwith "rotateZ not implemented"
  let sheareXY (distance : float) : transformation = failwith "sheareXY not implemented"
  let sheareXZ (distance : float) : transformation = failwith "sheareXZ not implemented"
  let sheareYX (distance : float) : transformation = failwith "sheareYX not implemented"
  let sheareYZ (distance : float) : transformation = failwith "sheareYZ not implemented"
  let sheareZX (distance : float) : transformation = failwith "sheareZX not implemented"
  let sheareZY (distance : float) : transformation = failwith "sheareZY not implemented"
  let scale (x : float) (y : float) (z : float) : transformation = failwith "scale not implemented"
  let mirrorX : transformation = failwith "mirrorX not implemented"
  let mirrorY : transformation = failwith "mirrorX not implemented"
  let mirrorZ : transformation = failwith "mirrorX not implemented"
  let mergeTransformations (ts : transformation list) : transformation = failwith "mergeTransformation not implemented"
  let transform (sh : shape) (tr : transformation) : shape = failwith "transform not implemented"
  let transformLight (l : light) (t : transformation) : light = failwith "transformLight not implemented"



  /////////////////////////////////
  // extended API for teams of 7 //
  /////////////////////////////////

  /// Type of acceleration structure
  type Acceleration = KDTree | RegularGrid | BVH

  /// Set which type of acceleration structure to use
  let setAcceleration (accel : Acceleration) : unit = failwith "setAcceleration not implemented"
