# Raytracer
Fully functional raytracer, written in F#. Made as the second-year project at IT-University of Copenhagen. 

![Raytracer](http://mhooge.com/misc/bunny-texture.png)

## Features
- Render images of basic shapes, such a rectangles and squares
- Realistic lights and shadows, including ambient lights, point lights, and directional lights
- Advanced lighting with area lights, environment lights, and ambient occlusion
- Different material types: matte, specular refective, glossy reflective, emmisive, transparent, and any combination of these
- Texture mapping: map any texture unto any shape and material
- Transformations: scale, translate, sheare or rotate any shape or object
- Sampling: sample average ray collisions at each pixel for improved visuals
- Thin-Lens camera: emulate real cameras with objects in focus being sharp, or blurred otherwise
- Triangle Meshes: import any .ply file of any complexity and render it realistically
- Implicit Surfaces: create complex shapes from complex polynomial equations
- Acceleration structures: raytracing is sped up by the use of either a KDTree, a BVH (Bounding Volume Hierachy) or regular grids
